package org.dbpedia.spotlight.relevance

import org.dbpedia.spotlight.model._
import scala.collection.mutable.ListBuffer
import org.dbpedia.spotlight.db.model.ContextStore
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Created by dav009 on 31/01/2014.
 */
trait Relevance {


  /*
  * Gets the tokens of a text, count the frequency of the stems found in them
  * prunes the frequency table up to the 100most common items
  * normalize the frequency table
  * returns a Map[TokenType, dobule]
  * */
  def getAllTextContextVector(textContext:List[Token]):Map[TokenType,Double]={

    var tokenCounts:Map[TokenType,Int] = textContext.groupBy(_.tokenType).mapValues(_.size)

    //removing uninteresting tokens
    if  (tokenCounts.contains(TokenType.UNKNOWN))
      tokenCounts = tokenCounts - TokenType.UNKNOWN

    if  (tokenCounts.contains(TokenType.STOPWORD))
      tokenCounts = tokenCounts - TokenType.STOPWORD

    // cut the vector of the text only allowing the tokens with highest counts
    val prunedVector = tokenCounts.toSeq.sortBy(_._2).reverse.slice(0,100).toMap

    // Normalizing the pruned Vector
    val normalizedVector:mutable.Map[TokenType,Double]= mutable.Map[TokenType,Double]()
    val totalCounts = prunedVector.values.toList.sum

    for( (token, counts) <- prunedVector){
      val normalizedCount = counts / totalCounts.toDouble
      normalizedVector(token) = normalizedCount
    }

   return normalizedVector.toMap
  }

  /*
  * Given a list of DbpediaTopics, it returns their contextVectors
  * */
  def getContextCounts(contextStore:ContextStore, listOfDbpediaResources:Iterable[DBpediaResource]):Map[DBpediaResource,Map[TokenType, Int]] ={
    val contextCounts = mutable.Map[DBpediaResource,Map[TokenType, Int]]()
    for(dbpediaResource<-listOfDbpediaResources){
      val currentCounts = contextStore.getContextCounts(dbpediaResource).toMap
      contextCounts(dbpediaResource) = currentCounts
    }
    return contextCounts.toMap
  }



  def calculateRelevance(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence], allText:Text):java.util.Map[DBpediaResource,java.lang.Double]
  //def calculateRelevance(listOfResourceOcurrence:List[DBpediaResourceOccurrence]):Map[DBpediaResourceOccurrence,Double]
}
