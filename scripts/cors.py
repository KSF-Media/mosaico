from mitmproxy import http


def response(flow: http.HTTPFlow):
    flow.response.headers["Access-Control-Allow-Origin"] = "*"


def request(flow: http.HTTPFlow) -> None:
    if flow.request.method == "OPTIONS":
        h = flow.request.headers
        origin = h['origin'] if 'origin' in h else '*'
        flow.response = http.Response.make(
            200, b"", {
                "Access-Control-Allow-Origin": origin,
                "Access-Control-Allow-Methods": "GET,POST",
                "Access-Control-Allow-Headers": "*",
                "Access-Control-Max-Age": "1728000"
            })
