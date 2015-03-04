package core;

/**
 * 
 */

/**
 * Bitboard data structure implementation of OthelloBoard
 * 
 * this data structure, although dense and hard to read, is needed for
 * 'industrial stength' rapid move generation and move execution.
 * 
 * Notations used in this file:
 * Rx : Row x on the board
 * Cx : Column x on the board
 * DA0 : Ascending Diagonal (no offset)
 * DD0 : Descending Diagonal (no offset)
 * 
 * @author Nicholas Ver Hoeve
 */
public class OthelloBitBoard implements OthelloBoard {
	public long white;
	public long black;
	
	/**
	 * construct new board with the initial game position
	 */
	public OthelloBitBoard() {
		newGame();
	}
	
	/**
	 * construct new board from a bitboard of each piece type
	 * 
	 * @param white : bitboard corrosponding to white piece placement
	 * @param black : bitboard corrosponding to black peice placement
	 */
	public OthelloBitBoard(long white, long black) {
		this.white = white;
		this.black = black;
	}

	/**
	 * Copy an OthelloBitBoard
	 * 
	 * @param toCopy
	 */
	public OthelloBitBoard(OthelloBitBoard toCopy) {
		white = toCopy.white;
		black = toCopy.black;
	}
	
	/**
	 * Copy an OthelloBoard
	 * 
	 * @param toCopy
	 */
	public OthelloBitBoard(OthelloBoard toCopy) {
		if (toCopy instanceof OthelloBitBoard) {
			OthelloBitBoard o = (OthelloBitBoard)toCopy;
			white = o.white;
			black = o.black;
		}
		
		//need to generalize if other implementations of OthelloBoard are ever made
	}
	
	/**
	 * merges x, y coordinate pairs into a single value, (0-63)
	 * 
	 * @param x : x coordinate, (0, 7)
	 * @param y : y coordinate, (0, 7)
	 * @return an index, in range 0-63
	 */
	static int xyMerge(int x, int y) {
		return x | (y << 3);
	}
	
	/**
	 * extracts x coordinate from an 'xy' int
	 * 
	 * @param xy : position (0-63)
	 * @return extracted x coordinate
	 */
	static int xyTox(int xy) {
		return xy & 7;
	}
	
	/**
	 * extracts y coordinate from an 'xy' int
	 * 
	 * @param xy : position (0-63)
	 * @return extracted y coordinate
	 */
	static int xyToy(int xy) {
		return xy >> 3;
	}
	
	/**
	 * remaps the bit values in C0 to be the bit values of R0
	 * 
	 * @param x : the bitboard
	 * @return new bitboard
	 */
	private static int mapC0toR0(long x) {
		x &= 0x0101010101010101L;
		x |= x >> 28;
		x |= x >> 14;
		x |= x >> 7;
		
		return (int)x & 0xFF;
	}
	
	/**
	 * remaps the bit values in DA0 (the ascending diagonal) to be the bit 
	 * values of R0
	 * 
	 * @param x : the bitboard
	 * @return new bitboard
	 */
	private static int mapDA0ToR0(long x) {
		x &= 0x8040201008040201L;
		x |= x >> 32;
		x |= x >> 16;
		x |= x >> 8;
		return (int)x & 0xFF;
	}
	
	/**
	 * remaps the bit values in DD0 (the descending diagonal) to be the bit 
	 * values of R0
	 * 
	 * @param x : the bitboard
	 * @return new bitboard
	 */
	private static int mapDD0ToR0(long x) {
		x &= 0x0102040810204080L;
		x |= x >> 32;
		x |= x >> 16;
		x |= x >> 8;
		return (int)x & 0xFF;
	}
	
	/**
	 * remaps the bit values in R0 to be the bit values of C0
	 * the output is empty except for C0.  
	 * 
	 * @param x : the bitboard
	 * @return new bitboard
	 */
	private static long mapR0toC0(int x) {
		x |= x << 7;
		x |= x << 14;
		long z = (long)x | ((long)x << 28);
		
		return z & 0x0101010101010101L;
	}
	
	/**
	 * remaps the bit values in R0 to be the bit values of DA0 
	 * (ascending diagonal)
	 * the output is empty except for DA0.  
	 * 
	 * @param x : the bitboard
	 * @return new bitboard 
	 */
	private static long mapR0toDA0(int x) {
		x |= x << 8;
		long z = (long)x | ((long)x << 16);
		z |= z << 32;
		
		return z & 0x8040201008040201L;
	}
	
	/**
	 * remaps the bit values in R0 to be the bit values of DD0 
	 * (descending diagonal)
	 * the output is empty except for DD0.  
	 * 
	 * @param x : the bitboard
	 * @return new bitboard 
	 */
	private static long mapR0toDD0(int x) {
		x |= x << 8;
		x |= (x & 0x1122) << 16;
		long z = (long)x | ((long)x << 32);
		
		return z & 0x0102040810204080L;
	}
	
	/**
	 * empties the board
	 */
	public void clear() {
		white = 0L;
		black = 0L;
	}

	/**
	 * @param state : set as either WHITE or BLACK
	 * @return the number of one color's pieces on the board
	 */
	public int countPieces(int state) {
		if (state == WHITE) {
			return BitUtil.countSetBits(white);
		} else {
			return BitUtil.countSetBits(black);
		}
	}
	
	/**
	 * @param state : set as either WHITE or BLACK
	 * @return true if this player can make a move
	 */
	public boolean canMove(int state) {
		for (long toTry = generateLikelyMoves(state); toTry != 0; toTry &= toTry-1) {
			int square = BitUtil.ulog2(BitUtil.lowSetBit(toTry));
			if (moveIsLegal(xyTox(square), xyToy(square), state)) {
				return true;
			}
		}
		
		return false;
	}

	/**
	 * @return true if the game is over (neither player can move)
	 */
	public boolean gameIsSet() {
		return !(canMove(WHITE) || canMove(BLACK));
	}

	/**
	 * Simply sets the state of the square (x, y). 
	 * Does not verify the legality of the move in any way
	 * 
	 * @param x : horizontal coordinate (0-7)
	 * @param y : vertical coordinate (0-7)
	 */
	public int getSquare(int x, int y) {
		if (((1L << xyMerge(x, y)) & white) != 0) return WHITE;
		if (((1L << xyMerge(x, y)) & black) != 0) return BLACK;
		return EMPTY;
	}

	/**
	 * Performs a move and all updating involved in an in-game move.
	 * The move is assumed to be legal.
	 * 
	 * @param x : proposed horizontal coordinate (0-7)
	 * @param y : proposed vertical coordinate (0-7)
	 * @param state : set as either WHITE or BLACK
	 */
	public void makeMove(int x, int y, int state) {
		OthelloBitBoard newBoard = copyAndMakeMove(x, y, state);
		white = newBoard.white;
		black = newBoard.black;
	}
	
	/**
	 * Creates a copy of the game with a move applied.
	 * Performs a move and all updating involved in an in-game move.
	 * The move is assumed to be legal.
	 * Aggressively optimized for speed.
	 * 
	 * @param x : proposed horizontal coordinate (0-7)
	 * @param y : proposed vertical coordinate (0-7)
	 * @param state : set as either WHITE or BLACK
	 * @return updated copy of the board
	 */
	public OthelloBitBoard copyAndMakeMove(int x, int y, int state) {
		long cColor; // current color (color of who's turn it is)
		long eColor; // enemy color
		
		if (state == WHITE) {
			cColor = white;
			eColor = black;
		} else {
			cColor = black;
			eColor = white;
		}
		
		int cRow;
		int eRow;
		
		//this mask has a set bit for each position on the board that will be
		//wholly unnaffected by this move. Affected bit positions are cleared.
		long unmodifiedMask = ~Rom.ALLDIRECTIONLOOKUP[xyMerge(x, y)];
		
		long finalCColor = cColor & unmodifiedMask;
		long finalEColor = eColor & unmodifiedMask;

		//compute row modifications
		cRow = (int)(cColor >>> (8*y)) & 0xFF;
		eRow = (int)(eColor >>> (8*y)) & 0xFF;
		cRow = computeRowEffect(cRow, eRow, x);
		eRow &= ~cRow;
		finalCColor |= ((long)cRow << (8*y));
		finalEColor |= ((long)eRow << (8*y));
	
		//compute column modifications
		cRow = mapC0toR0(cColor >>> x);
		eRow = mapC0toR0(eColor >>> x);
		cRow = computeRowEffect(cRow, eRow, y);
		eRow &= ~cRow;
		finalCColor |= mapR0toC0(cRow) << x;
		finalEColor |= mapR0toC0(eRow) << x;
		
		//compute DA0 modifications
		byte shiftDistance = (byte)((x - y) << 3);
		cRow = mapDA0ToR0(BitUtil.signedLeftShift(cColor, shiftDistance));
		eRow = mapDA0ToR0(BitUtil.signedLeftShift(eColor, shiftDistance));
		cRow = computeRowEffect(cRow, eRow, x);
		eRow &= ~cRow;
		finalCColor |= BitUtil.signedLeftShift(mapR0toDA0(cRow), (byte)-shiftDistance);
		finalEColor |= BitUtil.signedLeftShift(mapR0toDA0(eRow), (byte)-shiftDistance);
		
		//compute DD0 modifications
		shiftDistance = (byte)((7 - x - y) << 3); // distance needed to map to DD0
		cRow = mapDD0ToR0(BitUtil.signedLeftShift(cColor, shiftDistance));
		eRow = mapDD0ToR0(BitUtil.signedLeftShift(eColor, shiftDistance));
		cRow = computeRowEffect(cRow, eRow, x);
		eRow &= ~cRow;
		finalCColor |= BitUtil.signedLeftShift(mapR0toDD0(cRow), (byte)-shiftDistance);
		finalEColor |= BitUtil.signedLeftShift(mapR0toDD0(eRow), (byte)-shiftDistance);
		
		if (state == WHITE) {
			return new OthelloBitBoard(finalCColor, finalEColor);
		} else {
			return new OthelloBitBoard(finalEColor, finalCColor);
		}
	}

	/**
	 * Test to see if player 'state' (BLACK or WHITE) can move at (x, y)
	 * Aggressively optimized for speed.
	 * 
	 * @param x: proposed x coordinate
	 * @param y: proposed y coordinate
	 * @param state: WHITE or BLACK
	 * @return true if move is legal
	 */
	public boolean moveIsLegal(int x, int y, int state) {
		long cColor; // current color (color of who's turn it is)
		long eColor; // enemy color
		
		if (state == WHITE) {
			cColor = white;
			eColor = black;
		} else {
			cColor = black;
			eColor = white;
		}
		
		int cRow;
		int eRow;
		
		//check for capture on row
		cRow = (int)(cColor >>> (8*y)) & 0xFF;
		eRow = (int)(eColor >>> (8*y)) & 0xFF;
		if (computeRowEffect(cRow, eRow, x) != cRow) {
			return true;
		}
		
		//check for capture on column
		cRow = mapC0toR0(cColor >>> x);
		eRow = mapC0toR0(eColor >>> x);
		if (computeRowEffect(cRow, eRow, y) != cRow) {
			return true;
		}
		
		//check for capture on ascending diagonal
		byte shiftDistance = (byte)((x - y) << 3); // distance needed to map to DA0
		cRow = mapDA0ToR0(BitUtil.signedLeftShift(cColor, shiftDistance));
		eRow = mapDA0ToR0(BitUtil.signedLeftShift(eColor, shiftDistance));
		if (computeRowEffect(cRow, eRow, x) != cRow) {
			return true;
		}
		
		//check for capture on descending diagonal
		shiftDistance = (byte)((7 - x - y) << 3); // distance needed to map to DD0
		cRow = mapDD0ToR0(BitUtil.signedLeftShift(cColor, shiftDistance));
		eRow = mapDD0ToR0(BitUtil.signedLeftShift(eColor, shiftDistance));
		if (computeRowEffect(cRow, eRow, x) != cRow) {
			return true;
		}
		
		return false;
	}
	
	/**
	 * private table-lookup function. Given a row of friendly and enemy pieces,
	 * and a proposed square to move, what will happen to the row? The answer
	 * is stored in the table.
	 * 
	 * @param mRow : 8-bit bitboard corrosponding to friendly pieces
	 * @param eRow : 8-bit bitboard corrosponding to enemy pieces
	 * @param pos : proposed place to move
	 * @return the new mrow (friendly pieces) after making the move
	 */
	private int computeRowEffect(int mRow, int eRow, int pos) {
		return (int)Rom.ROWLOOKUP[mRow | (eRow << 8) | (pos << 16)] & 0xFF;
	}

	/**
	 * load the starting position of the game
	 */
	public void newGame() {
		black = 0x0000000810000000L;
		white = 0x0000001008000000L;
	}

	/**
	 * Simply sets the state of the square (x, y). 
	 * Does not verify the legality of the move in any way
	 * 
	 * @param x : horizontal coordinate (0-7)
	 * @param y : vertical coordinate (0-7)
	 */
	public void setSquare(int x, int y, int state) {
		long v = (1L << xyMerge(x, y));
		
		switch (state) {
		case WHITE:
			white |= v;
			break;
		case BLACK:
			black |= v;
			break;
		case EMPTY:
			v = ~v;
			white &= v;
			black &= v;
			break;
		}
	}
	
	/**
	 * generates a bitboard where each bit is 'likely' a legal move.
	 * 
	 * @param state : BLACK or WHITE
	 * @return all bits that are empty squares and next to an enemy piece
	 */
	public long generateLikelyMoves(int state) {
		long emptySpace = ~(white | black);
		
		if (state == WHITE) {
			return fillNeighbors(black) & emptySpace;
		} else {
			return fillNeighbors(white) & emptySpace;
		}
	}
	
	/**
	 * @param v : original bitboard
	 * @return new bitboard that is all of the adjacent bits to every bit in v
	 */
	private long fillNeighbors(long v) {
		v |= (v << 1) & 0xFEFEFEFEFEFEFEFEL;
		v |= (v << 8);
		v |= (v >> 1) & 0x7F7F7F7F7F7F7F7FL;
		v |= (v >> 8);
		return v;
	}
	
	/**
	 * @override
	 */
	public boolean equals(Object other) {
		if (!(other instanceof OthelloBitBoard)) {
			return false;
		}

		OthelloBitBoard o = (OthelloBitBoard)other;
		return white == o.white && black == o.black;
	}
	
	/**
	 * This technique is inspired by zobrist hashing. Zobrist hashing is too slow
	 * when we cannot do 'incremental updating', and Othello's moving style makes 
	 * sure we can't easily do that.
	 * 
	 * This is a technique that should run faster than a zobrist hash when hashing
	 * from scratch.
	 * 
	 * @return : a hash code
	 */
	public int hashCode() {
		int wlo = (int)white;
		int whi = (int)(white >>> 32);
		int blo = (int)black;
		int bhi = (int)(black >>> 32);
		
		return Rom.NOISEA[wlo & 0xFF] ^ Rom.NOISEB[(wlo >>> 8) & 0xFF] ^
			Rom.NOISEC[(wlo >> 16) & 0xFF] ^ Rom.NOISED[(wlo >>> 24) & 0xFF] ^ 
			Rom.NOISEC[~whi & 0xFF] ^ Rom.NOISEA[(~whi >>> 8) & 0xFF] ^
			Rom.NOISED[(~whi >> 16) & 0xFF] ^ Rom.NOISED[(~whi >>> 24) & 0xFF] ^ 
			Rom.NOISEC[blo & 0xFF] ^ Rom.NOISEA[(blo >>> 8) & 0xFF] ^
			Rom.NOISEB[(blo >> 16) & 0xFF] ^ Rom.NOISED[(blo >>> 24) & 0xFF] ^ 
			Rom.NOISED[~bhi & 0xFF] ^ Rom.NOISEB[(~bhi >>> 8) & 0xFF] ^
			Rom.NOISEA[(~bhi >> 16) & 0xFF] ^ Rom.NOISEB[(~bhi >>> 24) & 0xFF];
	}
	
	/**
	* @override
	*/
	public String toString() {
		return "[" + Long.toHexString(white) + ", " + Long.toHexString(black) + "]";
	}
	
	/**
	 * test-drive the bitboard engine
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		final int numTests = 40;
		
		OthelloBitBoard testBoardA = new OthelloBitBoard(0x00000010081C0000L, 0x0000000000020200L);
		OthelloBitBoard testBoardB = new OthelloBitBoard(0x1020000001801400L, 0x0018102204080000L);
		OthelloBitBoard testBoardC = new OthelloBitBoard(0x30C8A298A69C8870L, 0xF801000041000009L);
		OthelloBitBoard testBoardD = new OthelloBitBoard(0x00000038180C0001L, 0x0000000000030204L);
		OthelloBitBoard testBoardE = new OthelloBitBoard(0xFF9F8D1D0D0F07D9L, 0x004002828200C824L);
		OthelloBitBoard testBoardF = new OthelloBitBoard(0xFF00000000000000L, 0x00000000000000FFL);

		
		for (int i = 0; i < numTests; ++i) {
			Object output = null;
			Object expectedOutput = null;
			
			switch (i) {
			case 0:
				output = new Boolean(testBoardA.moveIsLegal(5, 5, BLACK));
				expectedOutput = new Boolean(true);
				break;
			case 1:
				output = new Boolean(testBoardA.moveIsLegal(5, 2, BLACK));
				expectedOutput = new Boolean(true);
				break;
			case 2:
				output = new Boolean(testBoardA.moveIsLegal(4, 2, BLACK));
				expectedOutput = new Boolean(false);
				break;
			case 3:
				output = new Boolean(testBoardA.moveIsLegal(2, 4, BLACK));
				expectedOutput = new Boolean(false);
				break;
			case 4:
				output = new Boolean(testBoardB.moveIsLegal(2, 5, WHITE));
				expectedOutput = new Boolean(true);
				break;
			case 5:
				output = new Boolean(testBoardB.moveIsLegal(3, 7, WHITE));
				expectedOutput = new Boolean(false);
				break;
			case 6:
				output = new Boolean(testBoardB.moveIsLegal(4, 4, WHITE));
				expectedOutput = new Boolean(true);
				break;
			case 7:
				output = new Boolean(testBoardB.moveIsLegal(0, 5, WHITE));
				expectedOutput = new Boolean(true);
				break;
			case 8:
				output = testBoardB.generateLikelyMoves(WHITE);
				expectedOutput = 0x2C046F5D7A160800L;
				break;
			case 9:
				output = new Integer(testBoardB.countPieces(BLACK));
				expectedOutput = new Integer(7);
				break;
			case 10:
				output = testBoardB.copyAndMakeMove(4, 4, WHITE);
				expectedOutput = new OthelloBitBoard(0x1030101001801400L, 0x0008002204080000L);
				break;
			case 11:
				output = testBoardB.copyAndMakeMove(2, 5, WHITE);
				expectedOutput = new OthelloBitBoard(0x1028040201801400L, 0x0010102004080000L);
				break;
			case 12:
				output = testBoardC.copyAndMakeMove(3, 3, BLACK);
				expectedOutput = new OthelloBitBoard(0x30888288a0948070L, 0xF84120104F080809L);
				break;
			case 13:
				output = testBoardC.copyAndMakeMove(7, 0, BLACK);
				expectedOutput = new OthelloBitBoard(0x30482218261C0800L, 0xF8818080C18080F9L);
				break;
			case 14:
				output = new Boolean(testBoardD.canMove(BLACK));
				expectedOutput = new Boolean(true);
				break;
			case 15:
				output = testBoardE.copyAndMakeMove(1, 0, WHITE);
				expectedOutput = new OthelloBitBoard(0xFF9F8D1D0D0F07DFL, 0x004002828200C820L);
				break;
			case 16:
				output = new Boolean(testBoardA.gameIsSet());
				expectedOutput = new Boolean(false);
				break;
			case 17:
				output = new Boolean(testBoardB.gameIsSet());
				expectedOutput = new Boolean(false);
				break;
			case 18:
				output = new Boolean(testBoardC.gameIsSet());
				expectedOutput = new Boolean(false);
				break;
			case 19:
				output = new Boolean(testBoardD.gameIsSet());
				expectedOutput = new Boolean(false);
				break;
			case 20:
				output = new Boolean(testBoardE.gameIsSet());
				expectedOutput = new Boolean(false);
				break;
			case 21:
				output = new Boolean(testBoardF.gameIsSet());
				expectedOutput = new Boolean(true);
				break;
			default:
				continue;
			}
			
			if (output == null) {
				break;
			}
			
			boolean fail = false;
			System.out.println("Test " + i + ":");
			
			if (!output.equals(expectedOutput)) {
				fail = true;
				System.out.println("\tOutput: " + output.toString());
				System.out.println("\tExpected Output: " + expectedOutput.toString());
			}
			
			if (!fail) {
				System.out.println("\tPassed!");
			}
			
		}
		
	}

}