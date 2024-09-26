let Prelude =
      https://prelude.dhall-lang.org/v20.1.0/package.dhall
        sha256:26b0ef498663d269e4dc6a82b0ee289ec565d683ef4c00d0ebdd25333a5a3c98

let Map = Prelude.Map.Type

let Email = { Type = { emailAddress : Text }, default = {=} }

let Emails = Map Text Email.Type

let PhoneNumber = { Type = { number : Text }, default = {=} }

let PublicInfo =
      { name :
          { full : Text
          , first : Text
          , last : Text
          , initials : Text
          , formal : Text
          }
      , emails : { default : Email.Type, others : Emails }
      }

let PrivateInfo =
      { emails : { others : Emails }
      , phoneNumbers :
          { default : PhoneNumber.Type, others : Map Text PhoneNumber.Type }
      }

let Me =
      { Type = { public : PublicInfo, private : Optional PrivateInfo }
      , default.private = None PrivateInfo
      }

in  Me /\ { Email, Emails, PhoneNumber, PublicInfo, PrivateInfo }
