package u05lab.ex2

import u05lab.ex2.ConferenceReviewing
import org.junit.Assert.*
import org.junit.*

class TestConferenceReviewing {
  val conferenceReviewing:ConferenceReviewing = ConferenceReviewingImpl()

  @Before
  def before(): Unit =
    conferenceReviewing.loadReview(10, Map((Question.RELEVANCE, 9), (Question.SIGNIFICANCE, 2),
      (Question.CONFIDENCE, 3), (Question.FINAL, 6)))
    conferenceReviewing.loadReview(10, Map((Question.RELEVANCE, 2), (Question.SIGNIFICANCE, 2),
      (Question.CONFIDENCE, 3), (Question.FINAL, 8)))
    conferenceReviewing.loadReview(9, Map((Question.RELEVANCE, 9), (Question.SIGNIFICANCE, 2),
      (Question.CONFIDENCE, 3), (Question.FINAL, 4)))

  @Test
  def testLoad(): Unit =
    assertEquals((10, Map((Question.RELEVANCE, 9), (Question.SIGNIFICANCE, 2),
      (Question.CONFIDENCE, 3), (Question.FINAL, 6))), conferenceReviewing.getList.head)

  @Test
  def testOrderedScores(): Unit =
    assertEquals(List(2,9), conferenceReviewing.orderedScores(10, Question.RELEVANCE))

  @Test
  def testFinalAverage(): Unit =
    assertEquals(7, conferenceReviewing.averageFinalScore(10), 0.1)

  @Test
  def testAcceptedArticles(): Unit =
    assertEquals(List(10), conferenceReviewing.acceptedArticles())

  @Test
  def testSortedAcceptedArticles(): Unit =
    assertEquals(List((10, 7.0)), conferenceReviewing.sortedAcceptedArticles())

  @Test
  def testAverageWeightedFinalScoreMap(): Unit =
    assertEquals(Map((9, 1), (10, 1.5)), conferenceReviewing.averageWeightedFinalScoreMap())

}
