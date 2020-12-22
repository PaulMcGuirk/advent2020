using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Games
{
    public class Combat
    {
        private readonly bool _isRecursive;
        private readonly Queue<int>[] _decks;

        /// <summary>
        /// Create a new non-recursive instance of Combat
        /// </summary>
        /// <param name="playerOneDeck">The initial cards for player one</param>
        /// <param name="playerTwoDeck">The initial cards for player two</param>
        public Combat(IEnumerable<int> playerOneDeck, IEnumerable<int> playerTwoDeck)
            : this(playerOneDeck, playerTwoDeck, false)
        { }

        /// <summary>
        /// Create a new instance of Combat
        /// </summary>
        /// <param name="playerOneDeck">The initial cards for player one</param>
        /// <param name="playerTwoDeck">The initial cards for player two</param>
        /// <param name="isRecursive">If <c>true</c>, the game will be recursive</param>
        public Combat(IEnumerable<int> playerOneDeck, IEnumerable<int> playerTwoDeck, bool isRecursive)
        {
            _decks = new Queue<int>[2]
            {
                new Queue<int>(playerOneDeck),
                new Queue<int>(playerTwoDeck)
            };
            _isRecursive = isRecursive;
        }

        /// <summary>
        /// Play a game of combat
        /// </summary>
        /// <returns>The winning score</returns>
        public long Play()
        {
            var winningDeck = RunGame();

            return Score(winningDeck);
        }

        /// <summary>
        /// Performs the actual simulation, returning the ID of the player that
        /// won
        /// </summary>
        /// <returns>The ID of the player that won</returns>
        private int RunGame(bool isSubGame = false)
        {
            if (isSubGame && _decks[0].Max() > _decks[1].Max()
                && _decks[0].Max() > _decks.Select(deck => deck.Count).Sum())
            {
                // the player with the highest card will win, unless they lose
                // the card due to a recursive game. However, if the highest
                // card is greater than the card count, that recurse can't
                // happen and so we can automatically declare the winner.
                // The exception is if we hit an infinte loop, then player
                // 0 wins. So, we can only take this shortcut to declare
                // when it would declare 0 a winner, since they would be a winner
                // if there was a loop anyway.
                // We only take the short cut during sub games, because for
                // the parent game, we need to actually get the card order
                return 0;
            }

            var visited = new HashSet<string>(); // game states that have been visited

            while (_decks.All(deck => deck.Any()))
            {
                var stateString = GetStateString();
                if (visited.Contains(stateString))
                {
                    // if we've seen this state before, immediately declare
                    // player 1 a winner in order to avoid infinite recursion
                    return 0;
                }
                visited.Add(stateString);

                var cards = _decks.Select(deck => deck.Dequeue()).ToList();

                int roundWinner;
                if (_isRecursive && _decks[0].Count >= cards[0] && _decks[1].Count >= cards[1])
                {
                    var subGame = new Combat(_decks[0].ToList().Take(cards[0]), _decks[1].ToList().Take(cards[1]), true);
                    roundWinner = subGame.RunGame(true);
                }
                else
                {
                    roundWinner = cards[0] > cards[1] ? 0 : 1;
                }

                _decks[roundWinner].Enqueue(cards[roundWinner]);
                _decks[roundWinner].Enqueue(cards[(roundWinner + 1) % 2]);
            }

            return _decks[0].Any() ? 0 : 1;
        }

        /// <summary>
        /// Get the score for a deck
        /// </summary>
        /// <param name="playerId">The ID of the player to score</param>
        /// <returns>The score</returns>
        private long Score(int playerId)
        {
            var deck = _decks[playerId];
            var numCards = deck.Count;

            return deck.Select((card, pos) => card * (numCards - pos)).Sum();
        }

        /// <summary>
        /// Get a string representation of the current state
        /// </summary>
        private string GetStateString()
            => string.Join(';', _decks.Select(deck => string.Join(',', deck)));
    }
}
