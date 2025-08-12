FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt install -y \
  texlive-base \
  texlive-binaries \
  texlive-extra-utils \
  texlive-fonts-recommended \
  texlive-humanities \
  texlive-latex-base \
  texlive-latex-recommended \
  texlive-luatex \
  texlive-pictures \
  texlive-plain-generic \
  texlive \
  texstudio-doc \
  texstudio-l10n \
  latexmk \
  pandoc

RUN apt update && apt install -y npm

RUN npm install --global mermaid-filter@1.4.6

ARG USERNAME=user
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME

USER $USERNAME

COPY compile.sh /

ENTRYPOINT /compile.sh
