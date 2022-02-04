import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           PlutusTx.Prelude
import           Wallet.Effects         as Effects
import qualified PlutusTx.Builtins      as Builtins
import           Plutus.V1.Ledger.Ada as Ada
import           Ledger.Tx              (scriptTxOut, ChainIndexTxOut)
import           Data.Map             as Map
import           Plutus.ChainIndex.Tx 
import           Ledger.Blockchain 
import Playground.Contract
import           Prelude              ((/), Float, toInteger, floor)
import           Cardano.Api hiding (Value, TxOut,Address)
import           Cardano.Api.Shelley hiding (Value, TxOut, Address)
import           Codec.Serialise hiding (encode)
import           Ledger.Address         as Add


minADA :: Value
minADA = Ada.lovelaceValueOf 2000000

-- you can change the price here by adjusting the fuciton "price"
price :: Value 
price = Ada.lovelaceValueOf 10000000

data ContractInfo = ContractInfo
    { walletOwner :: !PubKeyHash
    } deriving (Generic, ToJSON, FromJSON)


--Put the sellers PubKeyHash here in replacement of the current one
contractInfo = ContractInfo 
    { walletOwner = "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2"
    }

--purchase portion of the contract 
data LootBoxData
instance Scripts.ValidatorTypes LootBoxData where
    type instance RedeemerType LootBoxData = ()
    type instance DatumType LootBoxData = ()

--Checks the validatorHash to make sure the LootBox has tokens to host the transaction
--If never changed by the end of the project use the builtins script validator
{-# INLINABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator _ _ ctx = True

lootBox :: Scripts.TypedValidator LootBoxData
lootBox = Scripts.mkTypedValidator @LootBoxData
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash lootBox

validate :: Validator
validate = Scripts.validatorScript lootBox

valAddress :: Address
valAddress = Scripts.validatorAddress lootBox



--Start of minting portion of the smart contract

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy||])
    `PlutusTx.applyCode`
    (PlutusTx.liftCode pkh)

--dont need
curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data SellParams = SellParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    , mpPolicy    :: !CurrencySymbol
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''MintParams

--Start of endpoints
type SignedSchema = 
    Endpoint "lock" SellParams
     .\/ Endpoint "purchase" ()


lock :: AsContractError e => SellParams -> Contract w s e ()
lock mp =  do
-- make a recusive function that allows for the creation of a 1000 utxos of diffrent datums from a wallet (cheap if implemented correctly) 
        
    let tx         = Constraints.mustPayToTheScript () $ (Value.singleton (mpPolicy mp) (mpnameOfToken mp) (mpAmount mp)) <> minADA
    void (submitTxConstraints lootBox tx)

purchase :: () -> Contract w SignedSchema Text ()
purchase _ =  do
    utxos <- fundsAtAddressGeq valAddress (Ada.lovelaceValueOf 1)

    let redeemer = () 
        ppkh     = walletOwner contractInfo
        tx       = mustPayToPubKey (Add.PaymentPubKeyHash ppkh) price <> collectFromScript utxos redeemer
        
    void (submitTxConstraintsSpending lootBox utxos tx)

endpoints :: Contract () SignedSchema Text ()
endpoints = awaitPromise (lock' `select` purchase') >> endpoints
  where
    lock' = endpoint @"lock" lock
    purchase' = endpoint @"purchase" purchase

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []
