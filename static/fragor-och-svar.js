import { addHandler } from "./common";
const showOnPx = 100;
const backToTopButton = document.querySelector(".static-page__back-to-top");

const scrollContainer = () => {
  return document.documentElement || document.body;
};

addHandler(
  document,
  () => {
    if (scrollContainer().scrollTop > showOnPx) {
      backToTopButton.style.opacity = "100%";
    } else {
      backToTopButton.style.opacity = "0%";
    }
  },
  "scroll"
);
