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
  def sortedAcceptedArticles(): List[Pair[Int, Double]]
  def averageWeightedFinalScoreMap(): Map[Int, Double]
  def getList: List[Pair[Int, Map[Question, Int]]]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

case class ConferenceReviewingImpl() extends ConferenceReviewing:
  private var reviews: List[Pair[Int, Map[Question, Int]]] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews :+ new Pair[Int, Map[Question, Int]](article, scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val scores: Map[Question, Int] = Map(
        Question.RELEVANCE -> relevance,
        Question.SIGNIFICANCE -> significance,
        Question.CONFIDENCE -> confidence,
        Question.FINAL -> fin)
    reviews = reviews :+ new Pair[Int, Map[Question, Int]](article, scores)

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(f => f.getX == article).map(x => x.getY()(question)).sorted

  override def averageFinalScore(article: Int): Double =
    this.average(reviews.filter(x => x.getX == article)
      .map(f => f.getY()(Question.FINAL)))

  private def average(x: List[Double]): Double = x.sum / x.length

  override def acceptedArticles(): List[Int] =
    reviews.filter(x => averageFinalScore(x.getX) > 5 && hasMinRelevance(x.getX))
      .map(f => f.getX).distinct

  private def hasMinRelevance(article: Int): Boolean =
    reviews.filter(x => x.getX == article)
      .map(x => x.getY()(Question.RELEVANCE)).exists(x => x >= 8)

  override def sortedAcceptedArticles(): List[Pair[Int, Double]] =
    acceptedArticles().map(x => new Pair[Int, Double](x, averageFinalScore(x)))

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map(x => (x.getX, calculateWeightedScore(x.getY)))
      .groupMap(x => x._1)(x => x._2)
      .view
      .mapValues(v => v.sum / v.size).toMap

  private def calculateWeightedScore(value: Map[Question, Int]): Double =
    value(Question.CONFIDENCE)*value(Question.FINAL)/10

  override def getList: List[Pair[Int, Map[Question, Int]]] = reviews


