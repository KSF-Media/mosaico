import { TCString } from '@iabtcf/core';
import Cookies from 'js-cookie'

function _checkEncodedConsentCookie(consents, id) {
    if (Cookies.get('FCCDCF') == undefined) {
        return
    } else {
        _setDecodedConsentCookie(consents, Cookies.get('FCCDCF'))
        window.clearInterval(id)
    }
}

function _setDecodedConsentCookie(consents, encodedCookie) {
    try {
        const decodedTCString = TCString.decode(JSON.parse(encodedCookie)[3][0]);
        decodedTCString.purposeConsents.has(1) ? consents["store_cookies"] = "granted" : consents["store_cookies"] = "denied"
    } catch (err) {
        console.error("error when setting consent cookie", err)
    } finally {
        Cookies.set("consents", JSON.stringify(consents))
    }
}

export function startConsentCookieSetupJS() {
    let consents = { "store_cookies": "denied" }
    Cookies.set("consents", JSON.stringify(consents))
    if (Cookies.get('FCCDCF') == undefined) {
        const id = window.setInterval(function () { _checkEncodedConsentCookie(consents, id) }, 1000)
    } else {
        _setDecodedConsentCookie(consents, Cookies.get('FCCDCF'))
    }
}
