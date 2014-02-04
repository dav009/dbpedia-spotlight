package org.dbpedia.spotlight.relevance

import org.dbpedia.spotlight.model._
import scala.collection.mutable.ListBuffer
import scala.Predef._
import scala.collection.mutable
import scala.collection.JavaConverters._
import org.dbpedia.spotlight.db.model.ContextStore

/**
 * Created by dav009 on 03/02/2014.
 */
class RelevanceDistanceToTextContext(val contextStore:ContextStore)  extends Relevance  {



  def normalizeVector(vector:Map[TokenType,Int]):Map[TokenType,Double]={
     val totalSumOfTokens = vector.values.sum
     var normalizedVector = Map[TokenType,Double]()
    for( (token, counts) <- vector){
      val normalizedCount = counts / totalSumOfTokens.toDouble
      normalizedVector += (token -> normalizedCount)
    }
    return normalizedVector
  }

  def getCanonicalVector(vector:Map[TokenType,Double], allDimensions:Set[TokenType]):ListBuffer[Double]={

    val listOfAllUniqueDimensions = allDimensions.toSeq
    var newVector = ListBuffer[Double]()
      for(dimension <- listOfAllUniqueDimensions){

        if (vector.contains(dimension))
          newVector += vector.get(dimension).get
        else
          newVector += 0.0
      }
    return newVector

  }


  def pruneVectors(listOfCounts:Map[DBpediaResource,Map[TokenType,Int]]): Map[DBpediaResource,Map[TokenType,Int]]={

    var listOfVectors = Map[DBpediaResource,Map[TokenType,Int]]()
    for ( (dbpediaResource,currentContextCounts)<- listOfCounts){
      //sort by counts
      val maxCountSubContext = currentContextCounts.toSeq.sortBy(_._2).reverse.slice(0,100)
      // get full dimesions
      var newVector =   Map[TokenType, Int]()
      for ( (token, counts) <-maxCountSubContext){
        newVector += (token -> counts)
      }
      listOfVectors += (dbpediaResource ->newVector)
    }
    return listOfVectors
  }

  /* given a list of dbpedia resoruce vectors
    - normalizes each vector
    - reduce the number of dimensions to 100
   */
  def transformCountsToVectors(contextVector:Map[TokenType,Double] , listOfCounts:Map[DBpediaResource,Map[TokenType,Int]]):Map[DBpediaResource,Map[TokenType,Double]]={
    // prune dimensions
    val allDimensions: Set[TokenType] = contextVector.keySet


    var normalizedVectors = Map[DBpediaResource,Map[TokenType,Double]]()
    for ( (dbpediaResource,currentContextCounts)<- listOfCounts){
      normalizedVectors +=(dbpediaResource ->normalizeVector(currentContextCounts))
    }

    return normalizedVectors
  }

  def icf(allTextVector:Map[TokenType,Double],topicVectors:Map[DBpediaResource,Map[TokenType,Double]]):Map[TokenType, Double]={

   var icfMap = Map[TokenType, Double]()
    val totalDocs = topicVectors.size.toDouble
    for((tokenType:TokenType, textCounts:Double) <- allTextVector){
       var counts = 0
       for ((dbpediaResource, contextVector)<-topicVectors){
         if (contextVector.contains(tokenType))
           counts = counts + 1
       }
      if (counts > 0)
        icfMap += (tokenType -> totalDocs/counts )
      else
        icfMap += (tokenType -> 0.0)

    }
    return icfMap
  }

  def getRelevances(topicVectors:Map[DBpediaResource,Map[TokenType,Double]], contextVector:Map[TokenType,Double], icfMap:Map[TokenType, Double]):Map[DBpediaResource, Double]={
    val scores = mutable.HashMap[DBpediaResource, Double]()
    val numberOfTokensInCommon = mutable.HashMap[DBpediaResource, Double]()
    val allTokens = contextVector.keySet
    for (tokenType<-allTokens){
      val icfValue = icfMap.get(tokenType).get
      topicVectors.keys foreach { dbpediaTopic: DBpediaResource =>
        scores(dbpediaTopic) = scores.getOrElse(dbpediaTopic, 0.0) + topicVectors(dbpediaTopic).getOrElse(tokenType,0.0)
        if (topicVectors(dbpediaTopic).contains(tokenType)){
          numberOfTokensInCommon(dbpediaTopic) = numberOfTokensInCommon.getOrElse(dbpediaTopic, 0.0) + 1.0
        }
       }
    }

    val sum_of_priors:Double = topicVectors.keySet.map(_.prior).sum
    val firstScore = mutable.HashMap[DBpediaResource, Double]()

    topicVectors.keys foreach { dbpediaTopic: DBpediaResource =>
      if (numberOfTokensInCommon(dbpediaTopic)>0){
        firstScore(dbpediaTopic) = scores(dbpediaTopic)
        scores(dbpediaTopic) = scores(dbpediaTopic) / numberOfTokensInCommon(dbpediaTopic)}
      else{
        scores(dbpediaTopic) = 0.0
        firstScore(dbpediaTopic) = scores(dbpediaTopic)}

      println(dbpediaTopic.uri)
      println("\t prior: "+dbpediaTopic.prior)
      println("\t log prior: "+ breeze.numerics.log(dbpediaTopic.prior))
      println("\t numberOfCommonTokens"+ numberOfTokensInCommon(dbpediaTopic))
      println("\t first score: "+ firstScore(dbpediaTopic))
      println("\t original score: "+ scores(dbpediaTopic))
      scores(dbpediaTopic) = scores(dbpediaTopic) / (dbpediaTopic.prior / sum_of_priors)
      println("\t final score: "+ scores(dbpediaTopic))

    }

    return scores.toMap
  }

  def calculateRelevance(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence], allText:Text):Map[DBpediaResource,Double]={
    val setOfDbpediaTopics=mutable.Set[DBpediaResourceOccurrence]()
    for (resource<- listOfResourceOcurrence.asScala){
      setOfDbpediaTopics.add(resource)
    }
    val allTokens:List[Token] = allText.featureValue("tokens").get
    val contextVector:Map[TokenType,Double] =getAllTextContextVector(allTokens)
    val originalCounts = getContextCounts(contextStore,setOfDbpediaTopics.toList)
    val contextCounts = pruneVectors(originalCounts)
    val normalizedVectors = transformCountsToVectors(contextVector, contextCounts)
    val icfMap = icf(contextVector, normalizedVectors)

    val scores = getRelevances(normalizedVectors, contextVector, icfMap)
    println("relevance scores")
    for((dbpediaURI, score)<- scores){
      println(dbpediaURI.uri+"-"+score)
    }
    return scores
  }
}
