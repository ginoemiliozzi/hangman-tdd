import org.scalatest.{BeforeAndAfterEach, FunSuite}


class GameTest extends FunSuite {
  

  test("setNickname should throw an exception when nickname is empty string") {
    val desiredNickname = ""
    val initialGame = Game()
    val caught = intercept[NotValidNickException] {
      GameService.setNickname(desiredNickname, initialGame)
    }
    assert(caught.msg === "Empty string is not a valid nickname")
  }

  test("setNickname should throw an exception when nickname is longer than 15 chars") {
    val desiredNickname = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val initialGame = Game()
    val caught = intercept[NotValidNickException] {
      GameService.setNickname(desiredNickname, initialGame)
    }
    assert(caught.msg === "Nickname should not be longer than 15 chars")
  }

  test("setNickname should set desired nickname as nickname with a valid string") {
    val desiredNickname = "Juan"
    val initialGame = Game()
    val newGame = GameService.setNickname(desiredNickname, initialGame)
    assert(newGame.nickname === desiredNickname)
  }

  test("setWord should match with the only word in the list") {
    val words = List("PERRO")
    val initialGame = Game()

    val newGame = GameService.setWord(words, initialGame)
    assert(newGame.selectedWord === "PERRO")
  }

  test("setWord should prepare guessed letters list") {
    val words = List("PERRO")
    val initialGame = Game()
    val resultGame = GameService.setWord(words, initialGame)

    assert(resultGame.guessedLetters.forall(l => l.isEmpty))
    assert(resultGame.guessedLetters.size === 5)
  }

  test("setWord should set a word from the list when receiving one") {
    val words = List("PERRO", "GATO")
    val initialGame = Game()
    val newGame = GameService.setWord(words, initialGame)
    assert(words.contains(newGame.selectedWord))
  }

  test("setWord should set a word from the default list when not receiving any list") {

    val initialGame = Game()
    val newGame = GameService.setWord(game = initialGame)
    assert(GameService.availableWords.contains(newGame.selectedWord))
  }

  test("guessLetter should add letter to wrongLetters if selected word does not contain the letter") {

    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val letter = 'W'
    val resultGame = GameService.guessLetter(letter, gameWithWord)

    assert(resultGame.wrongLetters.size === 1)
    assert(resultGame.wrongLetters.contains(letter))
  }

  test("guessLetter should add the letter to guessedLetters " +
    "at the correct position if it is correct") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val letter = 'T'
    val newGame = GameService.guessLetter(letter, gameWithWord)
    assert(newGame.guessedLetters.head === Some('T'))
    assert(newGame.guessedLetters(1) === None)
    assert(newGame.guessedLetters(2) === None)
    assert(newGame.guessedLetters(3) === Some('T'))
  }

  test("guessWord should correctly fill guessedLetters if the word is correct") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val wordToGuess = "TEST"
    val resultGame = GameService.guessWord(wordToGuess, gameWithWord)

    assert(resultGame.guessedLetters === List(Some('T'), Some('E'), Some('S'), Some('T')))
  }

  test("guessWord should add the wrong letters to the wrongLetters list") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val wordToGuess = "TAIL"
    val resultGame = GameService.guessWord(wordToGuess, gameWithWord)

    assert(resultGame.wrongLetters.size === 3)
    assert(wordToGuess.forall(c =>
      resultGame.wrongLetters.contains(c) || resultGame.selectedWord.contains(c)
      )
    )
  }

  test("getResult should return GameLost if the word is not completed" +
    " and there are no lives left") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val resultGame = List('A', 'B', 'C', 'D', 'F', 'G', 'H', 'J').foldLeft(gameWithWord)((currentGame, l) =>
      GameService.guessLetter(l, currentGame))
    val result = GameService.getResult(resultGame)

    assert(result === GameLost)
  }

  test("getResult should return GameWon if the word is completed" +
    " and there are lives left") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val resultGame = List('T', 'E', 'S').foldLeft(gameWithWord)((currentGame, l) =>
      GameService.guessLetter(l, currentGame))
    val result = GameService.getResult(resultGame)

    assert(result === GameWon)
  }

  test("getResult should return GameInProgress if the word is not completed" +
    " and there are lives left") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val resultGame = List('T', 'G').foldLeft(gameWithWord)((currentGame, l) => GameService.guessLetter(l, currentGame))
    val result = GameService.getResult(resultGame)

    assert(result === GameInProgress)
  }

  test("getLives should return the correct amount of lives left") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)
    val resultGame = List('T', 'G').foldLeft(gameWithWord)((currentGame, l) =>
      GameService.guessLetter(l, currentGame))

    val result = GameService.getLives(resultGame)
    val expectedResult = resultGame.initialLives - 1

    assert(result === expectedResult)
  }

  test("getLives should return the same as initial lives if there was no play") {
    val initialGame = Game()
    val gameWithWord = GameService.setWord(List("TEST"), initialGame)

    val lives = GameService.getLives(gameWithWord)
    val expectedResult = gameWithWord.initialLives

    assert(lives === expectedResult)
  }

}
