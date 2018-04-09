type EmailAddr = String
type Name = String
type Company = String
type EventName = String

data User = User Name EmailAddr

data Partner = Partner Company EmailAddr

data Attendee = Attendee EventName EmailAddr

sendUserPasswordResetEmail (User name email) = do
  putStrLn ("TO: " ++ email)
  putStrLn ("Dear " ++ name)
  putStrLn ("(instructions)")

sendPartnerPasswordResetEmail (Partner company email) = do
  putStrLn ("TO: " ++ email)
  putStrLn ("Dear representative of " ++ company)
  putStrLn ("(instructions)")

sendAttendeePasswordResetEmail (Attendee eventName email) = do
  putStrLn ("TO: " ++ email)
  putStrLn ("Dear attendee of " ++ eventName)
  putStrLn ("(instructions)")


---


class EmailRecipient a where
  getAddressingTerm :: a -> String
  getEmail :: a -> EmailAddr

instance EmailRecipient User where
  getAddressingTerm (User name _) = name
  getEmail (User _ email) = email

instance EmailRecipient Partner where
  getAddressingTerm (Partner company _) =
    "representative of " ++ company
  getEmail (Partner _ email) = email

instance EmailRecipient Attendee where
  getAddressingTerm (Attendee company _) =
    "attendee of " ++ company
  getEmail (Attendee _ email) = email


sendPasswordResetEmail :: EmailRecipient a => a -> IO ()
sendPasswordResetEmail recipient = do
  putStrLn ("TO: " ++ getEmail recipient)
  putStrLn ("Dear " ++ getAddressingTerm recipient)
  putStrLn ("(instructions)")

useSendPasswordResetEmail = do
  sendPasswordResetEmail (User "Robert Parker" "bob@example.com")
  sendPasswordResetEmail (Partner "Microsoft" "sales@microsoft.com")
  sendPasswordResetEmail (Attendee "SXSW" "jenny@gmail.com")
