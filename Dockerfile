FROM fpco/stack-build
ADD ./ /app
WORKDIR /app
RUN stack build
ENTRYPOINT [ "stack", "exec", "ft-turing-exe" ]
