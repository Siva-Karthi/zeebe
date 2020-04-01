/**
 * Copyright Camunda Services GmbH and/or licensed to Camunda Services GmbH under
 * one or more contributor license agreements. See the NOTICE file distributed
 * with this work for additional information regarding copyright ownership.
 * Licensed under the Zeebe Community License 1.0. You may not use this file
 * except in compliance with the Zeebe Community License 1.0.
 */
package io.zeebe.el.impl.feel

import org.camunda.feel.context.Context.StaticContext
import org.camunda.feel.context.{Context, FunctionProvider}
import org.camunda.feel.syntaxtree._

class FeelFunctionProvider extends FunctionProvider {

  override lazy val functionNames: Iterable[String] = functions.keys
  private val functions: Map[String, List[ValFunction]] = Map(
    "appendTo" -> List(appendFunction),
    "cycle" -> List(cycleFunction, cycleInfiniteFunction)
  )

  override def getFunctions(name: String): List[ValFunction] = functions(name)

  private def appendFunction = ValFunction(
    params = List("x", "y"),
    invoke = {
      case List(ValContext(x), ValContext(y)) => append(x, y)
      case List(ValNull, y: ValContext) => y
      case List(x: ValContext, ValNull) => x
      case List(ValError(_), y: ValContext) => y // ignore variable not found error
      case List(e: ValError, _) => e
      case List(_, e: ValError) => e
      case args => ValError(s"expected two contexts but found '$args'")
    }
  )

  private def append(x: Context, y: Context): ValContext = {
    val mergedVariables = x.variableProvider.getVariables ++ y.variableProvider.getVariables

    ValContext(StaticContext(variables = mergedVariables))
  }

  private def cycleFunction = ValFunction(
    params = List("repetitions", "interval"),
    invoke = {
      case List(ValNull, ValDayTimeDuration(duration)) => ValString("R/%s".format(duration))
      case List(ValNull, ValYearMonthDuration(duration)) => ValString("R/%s".format(duration))
      case List(ValNumber(repetitions), ValDayTimeDuration(duration)) =>
        ValString("R%d/%S".format(repetitions.toInt, duration))
      case List(ValNumber(repetitions), ValYearMonthDuration(duration)) =>
        ValString("R%d/%S".format(repetitions.toInt, duration))
      case List(e: ValError, _) => e
      case List(_, e: ValError) => e
      case args => ValError(s"expected a repetitions (number) and an interval (duration) parameter, but found '$args'")
    }
  )

  private def cycleInfiniteFunction = ValFunction(
    params = List("interval"),
    invoke = {
      case List(ValDayTimeDuration(duration)) => ValString("R/%s".format(duration))
      case List(ValYearMonthDuration(duration)) => ValString("R/%s".format(duration))
      case List(e: ValError) => e
      case args => ValError(s"expected an interval (duration) parameter, but found '$args'")
    }
  )
}
