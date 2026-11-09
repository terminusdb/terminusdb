# Jepsen control node: Clojure + leiningen + the test SSH key. The project
# is volume-mounted at /jepsen so source edits don't need a rebuild.
FROM clojure:temurin-21-lein-bookworm

RUN apt-get update \
 && apt-get install -y --no-install-recommends openssh-client gnuplot graphviz xvfb xauth git \
 && rm -rf /var/lib/apt/lists/* \
 && mkdir -p /root/.ssh

COPY secret/id_rsa /root/.ssh/id_rsa
RUN chmod 600 /root/.ssh/id_rsa \
 && printf 'Host *\n  StrictHostKeyChecking no\n  UserKnownHostsFile /dev/null\n' \
      > /root/.ssh/config

ENV JAVA_TOOL_OPTIONS="-Djava.awt.headless=true"
WORKDIR /jepsen
CMD ["sh", "-c", "xvfb-run -a lein run test --nodes n1 --time-limit ${TIME_LIMIT:-120} --concurrency ${CONCURRENCY:-8} --max-writes-per-key ${MAX_WRITES_PER_KEY:-128}"]
