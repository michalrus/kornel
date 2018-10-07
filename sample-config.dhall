{ serverHost = "irc.freenode.com"
, serverPort = 6697
, usingSSL = True
, nick = "kornel"
, nickservPassword = [ ./nickserv.pass as Text ] : Optional Text
, httpSnippetsFetchMax = Natural/toInteger (100 * 1024)
, cleverBotApiKey = [ ./cleverbot.key as Text ] : Optional Text
, haskellBotNicks = [ "lambdabot" ]
, scalaBotNicks = [ "multibot", "multibot_", "multibot1", "multibot_1", "multibot2", "multibot_2" ]
, channels = [ "#kornel-test" ]
, logTraffic = False
}
