export function easterEgg() {
    const main = document.getElementById("mosaico-main-content");
    if (main)
	main.style.rotate = !!main.style.rotate ? "" : "180deg";
}
