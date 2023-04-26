from mitmproxy import http


def response(flow: http.HTTPFlow):
    flow.response.headers["Access-Control-Allow-Origin"] = "*"


def request(flow: http.HTTPFlow) -> None:
    flow.request.headers["Origin"] = "http://localhost:8000"
