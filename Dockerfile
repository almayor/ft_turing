FROM fpco/stack-build:lts-19.28
ADD ./ /app
WORKDIR /app
RUN stack build
ENTRYPOINT [ "stack", "exec", "ft-turing-exe" ]
