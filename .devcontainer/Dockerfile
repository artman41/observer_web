FROM erlang:18.3

RUN apt update -y; \
    apt install -y make sudo less wget curl vim;

RUN useradd -m vscode && \
    chsh vscode -s /bin/bash && \
    echo 'vscode ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers && \
    for alias in \
        'alias ll="ls -l"'; \
    do \
        echo $alias >> /home/vscode/.bash_aliases; \
    done
