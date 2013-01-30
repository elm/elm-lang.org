
-- Try this out on an iOS or Android device. For best results
-- use the "In Tab" compile option.

main = lift (flow down . map asText) Touch.touches