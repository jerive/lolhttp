package lol.http

import java.security.MessageDigest
import java.util.Base64

import cats.effect.IO
import fs2.{Pure, Stream}
import lol.http

object WebSocket {
  object Headers {
    val SecWebsocketAccept = h"Sec-WebSocket-Accept"
    val SecWebSocketKey = h"Sec-WebSocket-Key"
    val SecSebSocketVersion = h"Sec-WebSocket-Version"

    val ValueWebsocket = h"websocket"
    val ValueRfc6455 = h"13"
  }

  import Headers._

  def apply(request: Request, io: Stream[Pure,String] => Stream[Pure,String]): Response =
    Server.negotiate(request.headers) match {
      case Right(k) => SwitchingProtocol(ValueWebsocket, wsTransform(io))
        .addHeaders(
          http.Headers.Connection -> h"Upgrade",
          SecWebsocketAccept -> k
        )
      case Left(response) => response
    }

  private[http] object Server {
    val secret = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
    val base64 = Base64.getEncoder

    def fingerprint(key: String): String = base64.encodeToString(sha1((key + secret).getBytes))

    def sha1(bytes: Array[Byte]): Array[Byte] = {
      val digest = MessageDigest.getInstance("SHA-1")
      digest.reset()
      digest.update(bytes)
      digest.digest()
    }

    def negotiate(requestHeaders: Map[HttpString,HttpString]): Either[Response, HttpString] = {
      def badRequest(s:String) = Response(
        status = 400,
        content = Content.of(s.getBytes),
        headers = Map(
          http.Headers.ContentType -> h"text/plain"
        ),
        upgradeConnection = null
      )

      def version: Either[Response,HttpString] = requestHeaders.collectFirst {
        case (SecSebSocketVersion, ValueRfc6455) => Right(ValueRfc6455)
        case (SecSebSocketVersion, other) => Left(badRequest(s"Unsupported websocket version: $other"))
      }.getOrElse(Left(badRequest("Missing header Sec-WebSocket-Version")))

      def host: Either[Response,Unit] = requestHeaders.collectFirst {
        case (http.Headers.Host, _) => Right(())
      }.getOrElse(Left(badRequest("Missing header `Host: hostname`")))

      def upgrade: Either[Response,Unit] = requestHeaders.collectFirst {
        case (http.Headers.Upgrade, ValueWebsocket) => Right(())
      }.getOrElse(Left(badRequest("Missing header `Upgrade: websocket`")))

      def connection: Either[Response,Unit] = requestHeaders.collectFirst {
        case (http.Headers.Connection, h"Upgrade") => Right(())
      }.getOrElse(Left(badRequest("Missing header `Connection: upgrade`")))

      def webSocketKey: Either[Response,HttpString] = requestHeaders.collectFirst {
        case (SecWebSocketKey, key) => Right(key)
      }.getOrElse(Left(badRequest("Missing header Sec-WebSocket-Key")))

      for {
        _ <- version.right
        _ <- host.right
        _ <- upgrade.right
        _ <- connection.right
        key <- webSocketKey.right
      } yield HttpString(fingerprint(key.str))
    }
  }

  /**
    * Use https://github.com/Spinoco/fs2-http/blob/series/0.2/src/main/scala/spinoco/fs2/http/websocket/WebSocket.scala
    * Use https://tools.ietf.org/html/rfc6455#page-58
    */
  private def wsTransform(io: Stream[Pure,String] => Stream[Pure,String]): (Stream[IO,Byte]) => Stream[IO,Byte] = ???
}
