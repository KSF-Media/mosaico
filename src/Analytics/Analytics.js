export function _pushToDataLayer(metadata) {
  window.dataLayer = window.dataLayer || [];
  window.dataLayer.push(function () {
    this.reset();
  });
  let push_data = { event: "page_data" };
  push_data.title = metadata.title;
  push_data.publishingTime = metadata.publishingTime;
  push_data.authors = metadata.authors.split(", ");
  push_data.premium = metadata.premium;
  push_data.listTitle = metadata.listTitle;
  push_data.category = metadata.category;
  push_data.articleUuid = metadata.articleUuid;
  push_data.userCusno = metadata.userCusno;
  push_data.articleLength = metadata.articleLength;
  push_data.tags = metadata.tags.split(", ").map((x) => x.replaceAll('"', ""));
  window.pageAnalyticsMetadata = push_data;
  dataLayer.push(push_data);
}

export function _sendPageView() {
  window.dataLayer = window.dataLayer || [];
  window.dataLayer.push(function () {
    this.reset();
  });
  let push_data = { event: "page_data", pageUrl: window.location.href, pageTitle: document.title };
  window.pageAnalyticsMetadata = push_data;
  dataLayer.push(push_data);
}

export function _setUserVariable(cusno, hasActiveSubscription) {
  window.ksfUser = { cusno: cusno, hasActiveSubscription: hasActiveSubscription };
}
