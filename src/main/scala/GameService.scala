import scala.util.Random

object GameService {

  val availableWords: List[String] = List("WELL", "HELLO", "AWESOME", "FANTASTIC")

  def setNickname(desiredNick: String, game: Game): Game = {
    def isValid: Boolean = {
      desiredNick match {
        case _ if desiredNick.isEmpty =>
          throw NotValidNickException("Empty string is not a valid nickname")
        case _ if desiredNick.length > 15 =>
          throw NotValidNickException("Nickname should not be longer than 15 chars")
        case _ => true
      }
    }
    if(isValid)
      game.copy(nickname = desiredNick)
    else
      game

  }

  def setWord(words: List[String] = availableWords, game: Game): Game = {
    val wordForGame = words(Random.nextInt(words.size))
    game.copy(
      selectedWord = wordForGame,
      guessedLetters = wordForGame.map(_ => None).toList
    )
  }

  def guessLetter(letter: Char, game: Game): Game = {
    if(game.selectedWord.contains(letter)) {
      game.copy(guessedLetters =
        game.selectedWord.zipWithIndex.map{ case (l, idx) =>
          if(l.equals(letter)) Some(letter)
          else game.guessedLetters(idx)
        }.toList)
    }
    else {
      game.copy(wrongLetters = letter :: game.wrongLetters)
    }
  }

  def guessWord(word: String, game: Game): Game = {
    val isSameWord = word.equals(game.selectedWord)
    if(isSameWord) {
      game.copy(guessedLetters = word.map{ l => Some(l)}.toList)
    }
    else {
      game.copy(wrongLetters = addWrongLetters(word.toList, game.selectedWord, game.wrongLetters))
    }
  }

  def addWrongLetters(word: List[Char], selectedWord: String, wrongLetters: List[Char]): List[Char] = {

    def internalIter(str: List[Char], currentWrongLetters: List[Char]): List[Char] = {
      str match {
        case x :: xs if !currentWrongLetters.contains(x) && !selectedWord.contains(x) =>
          internalIter(xs, x :: currentWrongLetters)
        case _ :: xs =>
          internalIter(xs, currentWrongLetters)
        case Nil =>
          currentWrongLetters
      }
    }

    internalIter(word, wrongLetters)
  }

  def getLives(game: Game): Int = game.initialLives - game.wrongLetters.size

  def getResult(game: Game): GameStatus = {
    val isWordCompleted = game.guessedLetters.forall(l => l.nonEmpty)
    val livesLeft = (game.initialLives - game.wrongLetters.size) > 0
    if(isWordCompleted)
      GameWon
    else if(!livesLeft)
      GameLost
    else
      GameInProgress
  }
}