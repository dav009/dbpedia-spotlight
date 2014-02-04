package org.dbpedia.spotlight.relevance

import org.dbpedia.spotlight.model._
import scala.collection.mutable.ListBuffer
import org.dbpedia.spotlight.db.model.ContextStore
import scala.collection.JavaConversions._

/**
 * Created by dav009 on 31/01/2014.
 */
trait Relevance {


  // gets the stems of the tokesn and generate a frequency table which then is normalized and transformed
  //into a vector
  def getAllTextContextVector(textContext:List[Token]):Map[TokenType,Double]={
    val counts:Map[TokenType, Int]= Map[TokenType, Int]()
    val tokenCounts:Map[TokenType,Int] = textContext.groupBy(_.tokenType).mapValues(_.size)
    var normalizedTokenVector:Map[TokenType,Double]= Map[TokenType,Double]()
    val totalCounts = tokenCounts.values.toList.sum
    for (tokenType<- tokenCounts.keys){
      val normalizedValue = tokenCounts.get(tokenType).get / totalCounts.toDouble
      normalizedTokenVector += (tokenType -> normalizedValue)
    }
   return normalizedTokenVector
  }

  //given a list of dbpedia resources it returns a map:dbpediaResoruce -> freqiuecyTable of TokenTypes
  def getContextCounts(contextStore:ContextStore, listOfResourceOcurrence:List[DBpediaResourceOccurrence]):Map[DBpediaResource,Map[TokenType, Int]] ={
    var contextCounts = Map[DBpediaResource,Map[TokenType, Int]]()
    for(resourceOcurrence<-listOfResourceOcurrence){
      val currentCounts = contextStore.getContextCounts(resourceOcurrence.resource).toMap
      contextCounts += (resourceOcurrence.resource -> currentCounts)
    }
    return contextCounts
  }



  def calculateRelevance(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence], allText:Text):Map[DBpediaResource,Double]
  //def calculateRelevance(listOfResourceOcurrence:List[DBpediaResourceOccurrence]):Map[DBpediaResourceOccurrence,Double]
}
