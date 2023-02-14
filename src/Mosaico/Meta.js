export function deleteBySelector(selector) {
    return () => document.head.querySelectorAll(selector).forEach(e => e.remove());
}
export function appendToHead(html) {
    return () => document.head.innerHTML += html;
}
