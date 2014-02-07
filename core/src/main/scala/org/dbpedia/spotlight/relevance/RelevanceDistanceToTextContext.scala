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
    var normalizedVector = mutable.Map[TokenType,Double]()
    for( (token, counts) <- vector){
      val normalizedCount = counts / totalSumOfTokens.toDouble
      normalizedVector(token) = normalizedCount
    }

    return normalizedVector.toMap
  }



  def pruneVectors(listOfContextVectors:Map[DBpediaResource,Map[TokenType,Int]]): Map[DBpediaResource,Map[TokenType,Int]]={
    var prunedContextVectors = mutable.Map[DBpediaResource,Map[TokenType,Int]]()
    for ( (dbpediaResource,currentContextCounts)<- listOfContextVectors){
      // get the  100 dimensions with highest counts
      val prunedVector = currentContextCounts.toSeq.sortBy(_._2).reverse.slice(0,100).toMap
      prunedContextVectors(dbpediaResource) = prunedVector
    }
    return prunedContextVectors.toMap
  }

  /* given a list of dbpedia resoruce vectors
    - normalizes each vector
    - reduce the number of dimensions to 100
   */
  def transformCountsToVectors(contextVector:Map[TokenType,Double] , listOfContextVectors:Map[DBpediaResource,Map[TokenType,Int]]):Map[DBpediaResource,Map[TokenType,Double]]={

    var normalizedVectors = mutable.Map[DBpediaResource,Map[TokenType,Double]]()

    for ( (dbpediaResource,currentContextCounts)<- listOfContextVectors){
      normalizedVectors(dbpediaResource) = normalizeVector(currentContextCounts)
    }

    return normalizedVectors.toMap
  }

  def icf(allTextVector:Map[TokenType,Double],topicVectors:Map[DBpediaResource,Map[TokenType,Double]]):Map[TokenType, Double]={

   var icfMap = mutable.Map[TokenType, Double]()
    val totalDocs = topicVectors.size.toDouble
    for((tokenType:TokenType, textCounts:Double) <- allTextVector){
       var counts = 0
       for ((dbpediaResource, contextVector)<-topicVectors){
         if (contextVector.contains(tokenType))
           counts = counts + 1
       }
      if (counts > 0)
        icfMap(tokenType) = counts/totalDocs.toDouble
      else
        icfMap(tokenType) = 0.0

    }
    return icfMap.toMap
  }

  def getRelevances(topicVectors:Map[DBpediaResource,Map[TokenType,Double]], contextVector:Map[TokenType,Double], icfMap:Map[TokenType, Double], topicFrequencyInText:Map[DBpediaResource, Int]):Map[DBpediaResource, Double]={
    val scores = mutable.HashMap[DBpediaResource, Double]()
    val numberOfTokensInCommon = mutable.HashMap[DBpediaResource, Double]()
    val allTokens = contextVector.keySet

   // val matchedTokensToMatchedTopics = mutable.HashMap[TokenType, mutable.ListBuffer[DBpediaResource]]()

    for (tokenType<-allTokens){
      val icfValue = icfMap.get(tokenType).get
      topicVectors.keys foreach { dbpediaTopic: DBpediaResource =>
        val topicScore =  topicVectors(dbpediaTopic).getOrElse(tokenType,0.0)
        val boostScoreContext =  topicScore * contextVector.getOrElse(tokenType,0.0)
        val boostCommonTokenAmongTopics = topicScore  *  icfMap.getOrElse(tokenType,0.0) * 0.5
        scores(dbpediaTopic) =   scores.getOrElse(dbpediaTopic, 0.0) + topicScore + boostScoreContext + boostCommonTokenAmongTopics
        if (topicVectors(dbpediaTopic).contains(tokenType)){
          numberOfTokensInCommon(dbpediaTopic) = numberOfTokensInCommon.getOrElse(dbpediaTopic, 0.0) + 1.0
         // val currentMatchedTokenTopics = matchedTokensToMatchedTopics.getOrElse(tokenType, new mutable.ListBuffer[DBpediaResource]())
         // currentMatchedTokenTopics += dbpediaTopic
          //matchedTokensToMatchedTopics(tokenType) = currentMatchedTokenTopics
        }
       }
    }



    topicVectors.keys foreach { dbpediaTopic: DBpediaResource =>
      if (numberOfTokensInCommon.contains(dbpediaTopic)  && (numberOfTokensInCommon(dbpediaTopic) > 0)){
      }
      else{
        scores(dbpediaTopic) = 0.0
      }
    }


    //Boost based on the number of times a topic is spotted in the text
    val maxTopicFrequency:Int= topicFrequencyInText.values.map(_.toInt).max
    scores.keys foreach{ dbpediaTopic: DBpediaResource =>
      val boostByCounts =  (1 -scores(dbpediaTopic))*(topicFrequencyInText(dbpediaTopic)/maxTopicFrequency.toDouble)
      scores(dbpediaTopic) = scores(dbpediaTopic) + boostByCounts
    }


    //MinMaxNormalization
    //minmaxNorm
    var maxValue = -100.0
    var minValue = 10000000.0

    minValue = scores.values.min
    maxValue = scores.values.max


    val topScore = (maxValue + 2.0)/3.0

    scores.keys foreach{ dbpediaTopic: DBpediaResource =>
      //new min value score is 0.1
      //new max value is topScore
      scores(dbpediaTopic) = getMinMaxNormalizationValue(scores(dbpediaTopic), minValue, maxValue,0.1, topScore)
    }

    println("minValue:"+minValue)
    println("maxValue"+maxValue)
    println("FINAL SCORES::::::::")
    val orderedScores = scores.toSeq.sortBy(_._2)
    for( (dbpediaTopic, score)<-orderedScores ){
        println(dbpediaTopic.uri)
        println("\t"+score)
    }

    return scores.toMap
  }

  def getMinMaxNormalizationValue(currentValue:Double, minValue:Double, maxValue:Double, newMinValue:Double, newMaxValue:Double):Double ={
    return ((currentValue - minValue) / (maxValue-minValue)) * (newMaxValue-newMinValue) + newMinValue
  }

  def calculateRelevance(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence], allText:Text):java.util.Map[DBpediaResource,java.lang.Double]={

    val listOfDbpediaOcurrences=listOfResourceOcurrence.asScala

    //How many times a topic was spotted in the text
    val topicFrequencyInText = listOfDbpediaOcurrences.groupBy(_.resource).mapValues(_.size)


    val allTokens:List[Token] = allText.featureValue("tokens").get
    val contextVector:Map[TokenType,Double] =getAllTextContextVector(allTokens)
    val originalCounts = getContextCounts(contextStore,topicFrequencyInText.keys)

    val contextCounts = pruneVectors(originalCounts)
    val normalizedVectors = transformCountsToVectors(contextVector, contextCounts)
    val icfMap = icf(contextVector, normalizedVectors)

    val scores = getRelevances(normalizedVectors, contextVector, icfMap,topicFrequencyInText)
    println("relevance scores")
    for((dbpediaURI, score)<- scores){
      println(dbpediaURI.uri+"-"+score)
    }

    return scores.asJava.asInstanceOf[java.util.Map[DBpediaResource,java.lang.Double]]
  }
}
