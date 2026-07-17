# sudo docker build --progress plain -t pi .
# sudo docker run --rm -it -v ~/.config/pi:/home/node/.pi -v (pwd):/home/node/host --entrypoint bash pi
FROM node:24-slim
RUN apt update && apt install -y fd-find ripgrep
RUN apt install -y curl git jq python3 python3-pip python3-venv
RUN apt install -y fish gh python3-bs4 python3-jsonschema python3-mypy python3-requests
RUN npm i -g @earendil-works/pi-coding-agent
RUN printf "pass\npass\n" | passwd
WORKDIR /home/node/host
ENV GH_TELEMETRY=false
ENV PI_OFFLINE=1
USER node
ENTRYPOINT ["bash"]

