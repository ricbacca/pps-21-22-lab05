package u05lab.ex2

import scala.jdk.CollectionConverters.*

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): List[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]
  def getList: List[(Int, Map[Question, Int])]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

case class ConferenceReviewingImpl() extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews :+= (article, scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val scores: Map[Question, Int] = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin)
    reviews :+= (article, scores)

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(f => f._1 == article).map(x => x._2(question)).sorted

  override def averageFinalScore(article: Int): Double =
    this.average(reviews.filter(x => x._1 == article)
      .map(f => f._2(Question.FINAL)))

  private def average(x: List[Double]): Double = x.sum / x.length

  override def acceptedArticles(): List[Int] =
    reviews.filter(x => averageFinalScore(x._1) > 5 && hasMinRelevance(x._1))
      .map(f => f._1).distinct

  private def hasMinRelevance(article: Int): Boolean =
    reviews.filter(x => x._1 == article)
      .map(x => x._2(Question.RELEVANCE)).exists(x => x >= 8)

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    acceptedArticles().map(x => (x, averageFinalScore(x)))

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map(x => (x._1, calculateWeightedScore(x._2)))
      .groupMap(x => x._1)(x => x._2)
      .view
      .mapValues(v => v.sum / v.size).toMap

  private def calculateWeightedScore(value: Map[Question, Int]): Double =
    value(Question.CONFIDENCE)*value(Question.FINAL)/10

  override def getList: List[(Int, Map[Question, Int])] = reviews


