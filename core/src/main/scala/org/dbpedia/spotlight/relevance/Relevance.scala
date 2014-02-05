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

    val maxCountSubContext = tokenCounts.toSeq.sortBy(_._2).reverse.slice(0,100)

    var prunedVector =   Map[TokenType, Int]()
    for ( (token, counts) <-maxCountSubContext){
      prunedVector += (token -> counts)
    }

    var normalizedTokenVector:Map[TokenType,Double]= Map[TokenType,Double]()
    val totalCounts = prunedVector.values.toList.sum
    for (tokenType<- tokenCounts.keys){
      val normalizedValue = prunedVector.get(tokenType).get / totalCounts.toDouble
      normalizedTokenVector += (tokenType -> normalizedValue)
      println(tokenType.toString)
      println("\t"+normalizedValue)
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
