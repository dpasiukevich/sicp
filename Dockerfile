FROM schemers/mit-scheme

COPY . .
WORKDIR /chapter5/compiler
CMD ["mit-scheme", "--load", "compile_and_go.scm"]
