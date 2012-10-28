

-- Displays the approximate time of your click

main = lift asText $ sampleOn Mouse.clicks (Time.every 0.2)