package com.rallyhealth.vapors.core.dsl.factfilter

import com.rallyhealth.vapors.core.data.{NoFactsMatch, ResultSet}
import com.rallyhealth.vapors.core.evaluator

private[dsl] trait Evaluation extends Types {

  /**
    * The core logic of this DSL operates on an [[TerminalFactsExp]] query to produce a function that
    * filters the give list of [[Facts]] down to the best-fitting list of facts that satisfies
    * the given expression, and wraps them up in an [[ResultSet]].
    *
    * @param facts the non-empty list of facts on which to operate
    * @param query the FreeApplicative query that will be passed to the [[evaluator]]
    * @return an [[ResultSet]] that contains either the facts that satisfy the expression or [[NoFactsMatch]]
    */
  def evalWithFacts(facts: Facts)(query: TerminalFactsExp): ResultSet = {
    evaluator.eval(facts)(query)
  }
}
