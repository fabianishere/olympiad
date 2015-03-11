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
	
	i += x - 1 >= 0 ? !m[p - 1] : 0;
	i += x + 1 < BOARD_M ? !m[p + 1] : 0;
	i += y - 1 >= 0 ? !m[p - 9] : 0;
	i += y + 1 < BOARD_N ? !m[p + 9] : 0;

	m[p] += 2;
	i += x - 1 >= 0 && v + 1 == m[p - 1] ? liberties(m, p - 1) : 0;
	i += x + 1 < BOARD_M && v + 1 == m[p + 1] ? liberties(m, p + 1) : 0;
	i += y - 1 >= 0 && v + 1 == m[p - 9] ? liberties(m, p - 9) : 0;
	i += y + 1 < BOARD_N && v + 1 == m[p + 9] ? liberties(m, p + 9) : 0;
	m[p] -= 2;
	return i;
}

/*
 * Determine whether or not the the stone at the given point is in atari.
 */
int atari(int *m, int p)
{
	return liberties(m, p) == 1;
}

/*
 * Determine whether or not the stone at the given point is captured.
 */
int captured(int *m, int p)
{
	return liberties(m, p) == 0;
}

/*
 * Draws the game board.
 */
void draw(int *m) 
{
	for (int x = 0; x < BOARD_M; x++) {
		for (int y = 0; y < BOARD_N; y++)
			printf("---");
		printf("\n|");
		for (y = 0; y < BOARD_N; y++)
			printf("%i|", m[BOARD_M * x + y]);
		printf("\n");
	}
	for (int y = 0; y < BOARD_N; y++)
		printf("---");
	printf("\n");
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
	memset(m, 0, sizeof(m));
	m[0] = 1;
	m[1] = 1;
	m[9] = 2;
	m[10] = 2;	
	printf("%i\n", liberties(m, 0));
	draw(m);
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
		round++;
		fgets(l, 5, stdin);
	}
	return 0;
}
