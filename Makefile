build:
	stack build

repl:
	stack repl

graylog:
	docker run -t -p 9000:9000 -p 12201:12201 -p 12201:12201/udp graylog2/allinone
