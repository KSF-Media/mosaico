import { applyCredentials } from '../Mosaico.Cache.Pubsub.Init/index.js';

// Side effect: May alter process.env.GOOGLE_APPLICATION_CREDENTIALS
const projectId = applyCredentials();
const paper = (process.env.PAPER === 'ön' ? 'on' : process.env.PAPER) || "hbl"
const topic = 'aptoma-frontpage-'+paper;

import { PubSub } from '@google-cloud/pubsub';

export const enabled = !!projectId;

export function subscribeImpl(callback) {
    return async function() {
	const pubsub = new PubSub({projectId});
	const [subscription] = await pubsub
	      .topic(topic)
	      .createSubscription('mosaico-'+paper+"-"+Math.round(new Date().getTime()/1000)+
				  "-"+(Math.random().toString().substring(2)),
				  {expirationPolicy: { ttl: {seconds: 7*86400 }}});
	subscription.on('message', message => {
	    callback(message)();
	    // Pubsub may do redeliveries despite this so we need to
	    // check stamps as well.
	    message.ack();
	})
    };
};

export function categoryImpl(message) {
    if (message.attributes.category) {
	return message.attributes.category;
    }
    return null;
};

export function content(message) {
    return message.data;
};

export function maxAge(message) {
    return message.attributes.maxAge;
}

export function stampImpl(message) {
    // Messages from previous version of Lettera had no stamp field.
    if (message.attributes.stamp === undefined) {
	return null;
    } else {
	return new Date(message.attributes.stamp);
    }
}
