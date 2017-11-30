package lol.http

class WebSocketTests extends Tests {
  test("Fingerprint computation") {
    WebSocket.Server.fingerprint("dGhlIHNhbXBsZSBub25jZQ==") shouldBe "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
  }

  test("Request negotiation") {
    import WebSocket.Headers._
    import lol.http.Headers._
    WebSocket.Server.negotiate(Map(
      Host -> h"http.lol",
      Upgrade -> ValueWebsocket,
      Connection -> h"Upgrade",
      SecWebSocketKey -> h"dGhlIHNhbXBsZSBub25jZQ==",
      SecSebSocketVersion -> h"13"
    )) shouldBe Right(h"s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")
  }
}
