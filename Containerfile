FROM debian:bullseye-slim
RUN apt-get update && apt-get -y --no-install-recommends install \
      ghostscript netpbm sbcl texlive-latex-base \
 && rm -rf /var/lib/apt/lists/*
COPY tex2page.lisp container.sh /usr/local/bin/
RUN { echo '#!/bin/sh'; \
      echo '":"; export LISP=sbcl'; \
      cat /usr/local/bin/tex2page.lisp; \
    } >/usr/local/bin/tex2page \
 && chmod +x /usr/local/bin/tex2page \
 && rm -f /usr/local/bin/tex2page.lisp \
 && mv /usr/local/bin/container.sh /usr/local/bin/container \
 && chmod +x /usr/local/bin/container
WORKDIR /work
ENTRYPOINT ["container"]
