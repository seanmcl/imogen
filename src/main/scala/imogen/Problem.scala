package imogen

import argonaut.{DecodeResult, CodecJson, Parse}

import scala.io.Source

sealed trait Status
case object Theorem extends Status
case object NonTheorem extends Status

//import argonaut._
//import Argonaut._
//import scalaz._
//import Scalaz._
//
//object Status {
//  implicit def StatusCodecJson: CodecJson[Status] = {
//    CodecJson(x => argonaut.Argonaut.jString(x.toString), {
//      case "Theorem" => DecodeResult.ok(Theorem)
//      case "NonTheorem" => DecodeResult.ok(NonTheorem)
//      case cur => DecodeResult.fail(cur.toString, cur.history)
//    })
//  }
//}
//
//case class Problem(id: String, formula: Formula, status: Status)
//
//object Problem {
//
//  implicit def ProblemCodecJson: CodecJson[Problem] = {
//      argonaut.Argonaut.casecodec3(Problem.apply, Problem.unapply)("id", "formula", "status")
//  }
//
//  def parseFile(file: String): List[Problem] = {
//    Parse.decodeOption[List[Problem]](Source.fromFile(file).toString()).getOrElse(Nil)
//  }
//
//}
