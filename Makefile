.PHONY: all run

all:
	stack build --test

run: all
	stack exec kornel-exe -- --host irc.freenode.com --port 6697 --ssl --nick kornel_ --channels '#test123456,#test1234567' --verbose --http-snippets-fetch-max 102400 --haskell-bot-nicks lambdabot --scala-bot-nicks multibot_,multibot_1
