package org.dbpedia.spotlight.relevance

import org.dbpedia.spotlight.log.SpotlightLog
import org.dbpedia.spotlight.model._
import scala.collection.JavaConversions._
import org.dbpedia.spotlight.db.model.ContextStore
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala._
import scala.Predef._
import nak.cluster._

/**
 * Created by dav009 on 31/01/2014.
 */
class RelevanceCentroid(val contextStore:ContextStore) extends Relevance {


  //def calculateRelevance(listOfResourceOcurrence:List[DBpediaResourceOccurrence], completeText:Text):Map[DBpediaResourceOccurrence,Double]={

 // }

  def getContextCounts(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence]):Map[DBpediaResource,Map[TokenType, Int]] ={
    var contextCounts = Map[DBpediaResource,Map[TokenType, Int]]()
    for(resourceOcurrence<-listOfResourceOcurrence.toList){
      val currentCounts = contextStore.getContextCounts(resourceOcurrence.resource).toMap
      contextCounts += (resourceOcurrence.resource -> currentCounts)
    }
    return contextCounts
  }

  def transformCountsToVectors(listOfCounts:Map[DBpediaResource,Map[TokenType,Int]]):Map[DBpediaResource,ListBuffer[Double]] ={
    // prune dimensions

    var allDimensions = Set[TokenType]()
    var listOfVectors = Map[DBpediaResource,Map[TokenType,Double]]()

    for ( (dbpediaResource,currentContextCounts)<- listOfCounts){
      //sort by counts
      val maxCountSubContext = currentContextCounts.toSeq.sortBy(_._2).reverse.subList(0,100)
      var sumOfcounts = 0
      // get full dimesions
      for ( (token, counts) <-maxCountSubContext){
        allDimensions = allDimensions ++ Set(token)
        sumOfcounts += counts
      }

      // normalize
      var newVector =   Map[TokenType, Double]()
      for ( (token, counts) <-maxCountSubContext){
        val newCounts:Double = counts / (1.0*sumOfcounts)
        newVector += (token -> newCounts)
      }
      listOfVectors += (dbpediaResource ->newVector)
    }

    val listOfAllUniqueDimensions = allDimensions.toSeq

    var canonicalVectors = Map[DBpediaResource,ListBuffer[Double]]()

    for((dbpediaResource,vector:Map[TokenType,Double]) <- listOfVectors){
       var newVector = ListBuffer[Double]()
       for(dimension <- listOfAllUniqueDimensions){

          if (vector.containsKey(dimension))
            newVector += vector.get(dimension).get
          else
            newVector += 0.0
       }
      canonicalVectors += (dbpediaResource -> newVector)
    }


    return canonicalVectors
  }

  def findCentroid(){

  }

  def getDistanceToCentroid(canonicalVectors:Map[DBpediaResource,ListBuffer[Double]]){
      val vectorsToCluster =new ArrayBuffer[ Point](canonicalVectors.size)
      var index = 0
      for((dbpdiaResource, vector) <- canonicalVectors){
        vectorsToCluster.add(index,Point(ArrayBuffer(vector: _*)))
        index = index + 1
      }
      val kmeans=  new Kmeans(vectorsToCluster, EuclideanDistance, 0.0001, 100, false)
      val (dispersion, centroids) = kmeans.run(1,300)
      println("Centroid VALUE")
      println(centroids)
  }

  def calculateRelevance(listOfResourceOcurrence:java.util.List[DBpediaResourceOccurrence]):Map[DBpediaResourceOccurrence,Double]={

    val contextCounts = getContextCounts(listOfResourceOcurrence)
    val canonicalVectors = transformCountsToVectors(contextCounts)

    return Map[DBpediaResourceOccurrence,Double]()

  }
}
