package org.dbpedia.spotlight.relevance

import org.dbpedia.spotlight.model.{Text, DBpediaResourceOccurrence}

/**
 * Created by dav009 on 31/01/2014.
 */
trait Relevance {

  def calculateRelevance(listOfResourceOcurrence: java.util.List[DBpediaResourceOccurrence]):Map[DBpediaResourceOccurrence,Double]
  //def calculateRelevance(listOfResourceOcurrence:List[DBpediaResourceOccurrence]):Map[DBpediaResourceOccurrence,Double]
}
