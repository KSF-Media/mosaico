FROM node:18

RUN ln -sf /usr/share/zoneinfo/Europe/Helsinki /etc/localtime

COPY output /app/output
COPY dist /app/dist
COPY static /app/static
COPY package.json /app/
COPY yarn.lock /app/

WORKDIR app
RUN yarn install
CMD node --input-type=module -r dotenv/config -e 'import {main} from "./output/Main/index.js"; main();'
