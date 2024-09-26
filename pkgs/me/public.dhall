let Me = ./schema.dhall

let PublicInfo = Me.PublicInfo

let Email = Me.Email

let Emails = Me.Emails

in    { name =
        { first = "Roel"
        , full = "Roel Hemerik"
        , formal = "R.A.B. Hemerik"
        , initials = "R.A.B."
        , last = "Hemerik"
        }
      , emails =
        { default = Email::{ emailAddress = "ik@roelweb.com" }
        , others = [] : Emails
        }
      }
    : PublicInfo
