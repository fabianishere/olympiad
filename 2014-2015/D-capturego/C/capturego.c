/*
 * Copyright 2014 Fabian M.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *	http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BOARD_M 9
#define BOARD_N 9

/*
 * Represents the state.
 */
enum State {
	INITIAL,
	CORNER_TERMINATION;
} State;

/*
 * Create a string representation of a point.
 */
void marshall(int p, char *s) 
{
	s[0] = p / 10 + 'A';
	s[1] = p % 10 + '1';
	s[2] = '\0';
}

/*
 * Unmarshall the given string to a point.
 */
int unmarshall(char *s)
{
	return 10 * (s[0] - 'A') + s[1] - '1';
}

/*
 * Return the liberties of the stone at the current point.
 */
int liberties(int *m, int p)
{
	int x = p / 10;
	int y = p % 10;
	int v = m[p] - 1;
	int i = 0;
	
	if (v < 0)
		return 0;
	
	i += x - 1 >= 0 ? (!v == m[p - 1] - 1) : 1;
	i += x + 1 < BOARD_M ? (!v == m[p + 1] - 1) : 1;
	i += y - 1 >= 0 ? (!v == m[p - 10] - 1) : 1;
	i += y + 1 < BOARD_N ? (!v == m[p + 10] - 1) : 1;
	return i;
}

/*
 * Determine whether or not the the stone at the given point is in atari.
 */
int atari(int *m, int p)
{
	return liberties(m, p) == 3;
}

/*
 * Determine whether or not the stone at the given point is captured.
 */
int captured(int *m, int p)
{
	return liberties(m, p) == 4;
}

/*
 * Determine whether the stone is placed at a corner..
 */
int is_corner_move(int p)
{
	return p == 0 || p == 8 || p == 80 || p == 88;
}

/*
 * Main entry point of the program.
 */
int main(/*int argc, char *argv[]*/)
{
	char l[6];
	/*char s[3];*/
	int m[BOARD_M * BOARD_N];
	int p = 0;
	int round = 1;
	enum State state = INITIAL;
	fgets(l, 6, stdin);
	if (strcmp(l, "Start") == 0) {
		printf("E5\n");
		fflush(stdout);
		m[44] = 1;
		round++;
		fgets(l, 5, stdin);
	}
	while (strcmp(l, "Quit") != 0) {
		p = unmarshall((char *) &l);
		m[p] = 2;
		if (is_corner_move(p) && can_terminate_corner_move(p))
			
			
		round++;
		fgets(l, 5, stdin);
	}
	return 0;
}