module Semigroup where

import Prelude

import Control.Monad.Eff as Eff
import Control.Monad.Eff.Console as Eff.Console
import Data.Array as Array
import Data.Bifunctor as Bifunctor
import Data.Either as Either
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.List.NonEmpty as NonEmptyList
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Validation.Semigroup as Validation
import Global.Unsafe as Unsafe.Global
import Partial.Unsafe as Partial

--------------------------------------------------------------------------------
-- | Utility function to unsafely construct a regular expression from a pattern
-- | string.
-- |
-- | This will fail at runtime with an error if the pattern string is invalid.
unsafeRegexFromString :: String -> Regex.Regex
unsafeRegexFromString str =
  let regex = Regex.regex str Regex.Flags.noFlags
  in Partial.unsafePartial (Either.fromRight regex)

--------------------------------------------------------------------------------
-- | Regular expression for email address validation.
emailRegex :: Regex.Regex
emailRegex =
  unsafeRegexFromString "^\\w+([.-]?\\w+)*@\\w+([.-]?\\w+)*(\\.\\w{2,3})+$"

-- | Regular expression for special symbols.
passwordRegex :: Regex.Regex
passwordRegex = unsafeRegexFromString "\\W"

-- | Minimum password length.
passwordMinLength :: Int
passwordMinLength = 8

--------------------------------------------------------------------------------
data ValidationError
  = EmptyField
  | InvalidEmailAddress
  | NoSpecialCharacter
  | LessThanMinLength

-- | Derive a `Generic` instance for `ValidationError` so we can get a
-- | `Show` instance to print to the console.
derive instance genericValidationError :: Generic.Generic ValidationError _

-- | Derive `show` for `ValidationError` using the `Generic` instance.
instance showValidationError :: Show ValidationError where
  show = Generic.Show.genericShow

type ValidationErrors = NonEmptyList.NonEmptyList ValidationError

--------------------------------------------------------------------------------
-- | Validate that the field of a form is non-empty.
validateNonEmpty :: String -> Validation.V ValidationErrors String
validateNonEmpty str
  | String.null str = Validation.invalid $ NonEmptyList.singleton EmptyField
  | otherwise = pure str

-- | Validate that the field of a form is a valid email address.
validateEmailRegex :: String -> Validation.V ValidationErrors String
validateEmailRegex email
  | Regex.test emailRegex email = pure email
  | otherwise = Validation.invalid $ NonEmptyList.singleton InvalidEmailAddress

-- | Validate that the field of a form has at least one special character.
validatePasswordRegex :: String -> Validation.V ValidationErrors String
validatePasswordRegex password
  | Regex.test passwordRegex password = pure password
  | otherwise = Validation.invalid $ NonEmptyList.singleton NoSpecialCharacter

-- | Validate that the field of a form is longer than `passwordMinLength`.
validatePasswordMinLength :: String -> Validation.V ValidationErrors String
validatePasswordMinLength password
  | String.length password > passwordMinLength = pure password
  | otherwise = Validation.invalid $ NonEmptyList.singleton LessThanMinLength

--------------------------------------------------------------------------------
-- | Sum type containing errors we could potentially encounter while validating
-- | the form.
data FormError' a
  = BadEmail a
  | BadPassword a

-- | Derive a `Functor` instance for `FormError'` so we can `map` into it.
derive instance functorFormError :: Functor FormError'

-- | Derive a `Generic` instance for `FormError'` so we can get a
-- | `Show` instance to print to the console.
derive instance genericFormError :: Generic.Generic (FormError' a) _

-- | Derive `show` for `FormError'` using the `Generic` instance.
instance showFormError :: Show a => Show (FormError' a) where
  show = Generic.Show.genericShow

-- | Type alias for a simple `FormError`, containing only `ValidationErrors`.
type FormError = FormError' ValidationErrors

-- | Type alias for a non-empty list of `FormError`s.
type FormErrors = NonEmptyList.NonEmptyList FormError

--------------------------------------------------------------------------------
-- | Validate that the field of a form is non-empty and has a valid email
-- | address.
validateEmail :: String -> Validation.V FormError String
validateEmail email = Bifunctor.lmap BadEmail $
     validateNonEmpty email
  *> validateEmailRegex email

-- | Validate that the field of a form is non-empty, has at least one special
-- | character, and is longer than `passwordMinLength`.
validatePassword :: String -> Validation.V FormError String
validatePassword password = Bifunctor.lmap BadPassword $
     validateNonEmpty password
  *> validatePasswordRegex password
  *> validatePasswordMinLength password

--------------------------------------------------------------------------------
-- | Type alias for a record that will represent our simple input form.
type Form =
  { email    :: String
  , password :: String
  }

-- | Validate that a form contains a valid email and a valid password.
validateForm :: Form -> Validation.V FormErrors Form
validateForm {email, password} = {email: _, password: _}
  <$> (Bifunctor.lmap NonEmptyList.singleton $ validateEmail email)
  <*> (Bifunctor.lmap NonEmptyList.singleton $ validatePassword password)

--------------------------------------------------------------------------------
-- | An empty form; this will parse as invalid.
testForm1 :: Form
testForm1 = {email: "", password: ""}

-- | A form with a bad email and a bad password; invalid.
testForm2 :: Form
testForm2 = {email: "bademail", password: "badpassword"}

-- | A form with a good email and a bad password; invalid.
testForm3 :: Form
testForm3 = {email: "good@email.com", password: "badpassword"}

-- | A form with a good email and a password that is too short; invalid.
testForm4 :: Form
testForm4 = {email: "good@email.com", password: "abc123+"}

-- | A form with a good email and a good password; valid.
testForm5 :: Form
testForm5 = {email: "good@email.com", password: "abc123+-="}

--------------------------------------------------------------------------------
main :: ∀ e. Eff.Eff (console :: Eff.Console.CONSOLE | e) Unit
main = do
  -- > Invalid [(BadEmail [EmptyField,InvalidEmailAddress]),(BadPassword [EmptyField,NoSpecialCharacter,LessThanMinLength])]
  Eff.Console.logShow $ formatValidationOutput $ validateForm testForm1

  -- > Invalid [(BadEmail [InvalidEmailAddress]),(BadPassword [NoSpecialCharacter])]
  Eff.Console.logShow $ formatValidationOutput $ validateForm testForm2

  -- > Invalid [(BadPassword [NoSpecialCharacter])]
  Eff.Console.logShow $ formatValidationOutput $ validateForm testForm3

  -- > Invalid [(BadPassword [LessThanMinLength])]
  Eff.Console.logShow $ formatValidationOutput $ validateForm testForm4

  -- > Valid "{\"email\":\"good@email.com\",\"password\":\"abc123+-=\"}"
  Eff.Console.logShow $ formatValidationOutput $ validateForm testForm5

  where
    -- Format the output of our validator.
    formatValidationOutput =
      Bifunctor.bimap
      -- Convert the `NonEmptyList` of `ValidationError` to an `Array`, eliminate any
      -- duplicate validation errors, and convert the `NonEmptyList` of `FormError`s
      -- to an `Array` too for easier printing
      (Array.fromFoldable <<< ((map <<< map) (Array.fromFoldable)))
      -- Unsafe stringify the record, in lieu of a `Show` instance.
      (Unsafe.Global.unsafeStringify)
