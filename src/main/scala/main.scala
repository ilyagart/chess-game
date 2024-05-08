package com.whitehatgaming

import processor.MoveProcessorImpl
import validation.MoveValidatorImpl

import com.whitehatgaming

import java.nio.file.{FileSystems, Files}
import scala.jdk.CollectionConverters.*

@main
def main(): Unit = {
  val validator = MoveValidatorImpl()
  val processor = MoveProcessorImpl(validator)
  val dir = FileSystems.getDefault.getPath("src/main/scala/data")
  val inputData: List[String] = Files.list(dir).iterator().asScala.toList.map(_.toAbsolutePath.toString)
  processor.process(inputData)
}