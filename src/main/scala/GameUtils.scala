case class Game( initialLives: Int = 7,
                 nickname: String = "",
                 selectedWord: String = "",
                 wrongLetters: List[Char] = List.empty[Char],
                 guessedLetters: List[Option[Char]] = List.empty[Option[Char]]
               )

trait GameStatus
case object GameWon extends GameStatus
case object GameLost extends GameStatus
case object GameInProgress extends GameStatus

case class NotValidNickException(msg: String) extends Exception
