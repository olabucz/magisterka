#include <iostream>
#include <algorithm>
#include <cmath>
#include <vector>
#include <fstream>
#include <string>
#include <sstream>
using namespace std;

//FUNKCJE:

//funkcja tworzy wektor cytowań i na jego podstawie oblicza wartość indeksu Hirscha
int oblicz_indeks_h(vector <int> vect, int h) { //jako argumenty przyjmuje wektor cytowań, na podstawie którego obliczany jest indeks h oraz
												//oznaczenie, czy należy odjąć 1 od wektora (dla "ogólnego" indeksu Hirscha), podanie -1 oznacza potrzebę odjęcia
	int h_cytowania = 0;
	if(vect.size() != 0) //by indeks Hirscha był w ogóle różny od 0
	{
		//wektory cytowań: - ile cytowań uzyskała każda z publikacji
		int Nmax = *std::max_element(vect.begin(), vect.end());
		
		//tablica - wektor cytowań
		int cytowania[Nmax];
		for(int i = 0; i < Nmax; i++)
		{
			cytowania[i] = count(vect.begin(), vect.end(), i+1);		
		}
		
		//jeśli ogólny indeks Hirscha (nie z autocytowań, bądź cytowań obcych) - odjęcie liczby 1 od każdej z wartości:
		if(h == (-1))
		{
			for( int i = 0; i < Nmax; i++) 
			{
				cytowania[i] = cytowania[i] - 1;
			}
		}
		
		//wypisywanie tablic - sprawdzenie:
		/*cout<<"Wektor cytowan: "<<endl;
		for( int i = 0; i < Nmax; i++ )
		{
			cout << cytowania[ i ]<<" ";
		}
		cout<<endl;*/
		
		//obliczanie indeksu Hirscha:
		sort(cytowania, cytowania + Nmax, greater < int >() );
		
		for(int l = 0; l < Nmax; l++)
		{
			if(cytowania[l] >= (l + 1)) // (l + 1), ponieważ iteracja tablicy od 0
			{
				h_cytowania++;
			}
		}
	}
		
	return h_cytowania;
}

//funkcja oblicza wartość średnią indeksu Hirscha po 100 próbach
double oblicz_sredni_h(int indeksy[]){
	double suma_h = 0; 
	for (int m = 0; m < 100; m++)
	{
		suma_h += indeksy[m];
	}
	
	double wart_sr = suma_h/100.0;
	
	return wart_sr;	
}

//funkcja oblicza wartość niepewności dla średniej wartości indeksu Hirscha po 100 próbach
double oblicz_odchstd_h(int indeksy[], double srednia){
	double odch_std_suma = 0; 
	for (int d = 0; d < 100; d++)
	{
		odch_std_suma += (indeksy[d] - srednia)*(indeksy[d] - srednia);
	}
	double odch_std = sqrt(odch_std_suma/(99*100));
	
	return odch_std;
}

//funkcja wczytuje parametr N z pliku i zapisuje go do wektora
vector <int> wczytaj_N(string sciezka) { 
	//wczytanie wartości z pliku:
	ifstream file(sciezka);	//ścieżka do pliku uzyskanego z R-a
	string str;
	vector <int> x;
	
	if(file.is_open())
	{	
		while (std::getline(file, str))
		{
			istringstream iss(str);
			string id;
			string N;
			string M;
			
			iss >> id >> N >> M;
			x.push_back(stoi(N));
		}
	}
	else
	{
		cout<<"Blad przy otwarciu pliku!"<<endl;
	}
	file.close();
	return x;
}

//funkcja wczytuje parametr M z pliku i zapisuje go do wektora
vector <int> wczytaj_M(string sciezka) { 
	//wczytanie wartości z pliku:
	ifstream file(sciezka);	//ścieżka do pliku uzyskanego z R-a
	string str;
	vector <int> x;
	
	if(file.is_open())
	{	
		while (std::getline(file, str))
		{
			istringstream iss(str);
			string id;
			string N;
			string M;
			
			iss >> id >> N >> M;
			x.push_back(stoi(M));
		}
	}
	else
	{
		cout<<"Blad przy otwarciu pliku!"<<endl;
	}
	file.close();
	return x;
}

int main ()
{
	srand (time(NULL));
	
	//parametry modelu IC:
	int No = 0;
	int q = 0;
	int p = 0;
	
	//podanie wartości powyższych parametrów przez użytkownika:
	cout<<"Podaj ponizsze wartosci: "<<endl;
	cout<<"Liczba cytowan wlasnych na krok czasowy - p: ";
	cin>>p;
	cout<<"Liczba cytowan obcych na krok czasowy  - q: ";
	cin>>q;
	
	//wczytanie z pliku parametrów N i M:
	vector <int> N;
	N = wczytaj_N("pre_2019_NM_do_symulacji.txt");
	vector <int> M;
	M = wczytaj_M("pre_2019_NM_do_symulacji.txt");
	
	//rozmiar segmentu początkowego:
	No = p + q;
	
	//plik do zapisu wyników:
	ofstream myfile("wyniki_pre.txt");
	
	//+++++warto sprawdzić, czy rozmiary N i M są takie same
	for (int ii = 0; ii < N.size(); ii ++)
	{
		//indeksy Hirscha:
		int h_cytowania = 0;
		int h_autocytowania = 0;
		int h_obcecytowania = 0; //jeśli parametry N i M na to nie pozwolą, to wartości pozostaną równe 0 - poza 
				
		//wartości niepewności:
		double odchstd_h = 0.0;
		double odchstd_h_autocyt = 0.0;
		double odchstd_h_obcecyt = 0.0;			
				
		if((N[ii] >= No) && (M[ii] != 0)) //by h miało w ogóle jakąś wartość -> N < No - program się nie wykona,  M = 0 - nie ma sensu, by program się wykonywał, wszystkie indeksy h = 0
		{
			//wektory:
			vector <int> w;
			vector <int> w_autocyt;
			vector <int> w_obcecyt;
					
			//tablice dla 100 prób - w celu uśrednienia wyników: (tu, a nie wyżej, bo skoro h=0, to nie ma sensu robić 100x obliczeń dla zera) 
			int indeksy[100];
			int indeksy_autocyt[100];
			int indeksy_obcecyt[100];
					
			for (int k = 0; k < 100; k++)
			{
				//segment początkowy:
				for (int i = 0; i < No; i++)
				{
					w.push_back(i + 1);
				} 
			
				//uzupełnianie tablicy cytowaniami i publikacjami:
				if ((p + q)*(N[ii] - No) >= M[ii]) //Przypadek 1 - wyczerpanie zbioru cytowań: - czyli nie są dodawane już potem publikacje, bo i tak nie zdobędą cytowań
				{
					cout<<(p + q)*(N[ii] - No)<<endl;
					cout<<M[ii]<<endl;
					cout<<"Przypadek 1 - wyczerpanie zbioru cytowań"<<endl;
					if((int (M[ii]/(p + q))) != 0) //???!!!
					{
						for (int i = 0; i < int (M[ii]/(p + q)); i++)
						{
							for (int j = 0; j < (p + q); j++) //obręb segmentu
							{
								w.push_back(w[rand() % (w.size() - j)]); 	// rand() % w.size() -j - dostęp do losowego indeksu, przy rosnącej dlugosci wektora, ale (- j) zapewnia stałą obszar losowania dla danego segmentu			
								if(j < p) //pierwsze p rozdanych cytowań traktujemy jako cytowania własne
								{
									w_autocyt.push_back(w[w.size()- 1]); //wpisuje do tablicy tę samą wartość, co dodana przed chwilą do wektora w
								}
								else //cytowania za p pierwszymi cytowaniami (własnymi) to cytowania obce
								{
									w_obcecyt.push_back(w[w.size()- 1]);
								}
							}
							w.push_back(No + (i + 1)); 
						}
					}	
					//ostatni segment: //tu trzeba coś zrobić z AUTOcytowaniami - tak jak wyżej, po prostu pierwsze p cytowań z niepełnego segmentu to autocytowania
					for (int j = 0; j < int (M[ii]%(p + q)); j++)
					{
						w.push_back(w[rand() % (w.size() - j)]); 	
						if(j < p) //pierwsze p rozdanych cytowań traktujemy jako cytowania własne
						{
							w_autocyt.push_back(w[w.size()- 1]);
						}	
						else
						{
							w_obcecyt.push_back(w[w.size()- 1]);
						}				
					}
				}
				else if ((p + q)*(N[ii] - No) < M[ii]) //Przypadek 2 - wyczerpanie zbioru publikacji: - czyli za ostatnią dodaną publikacją będą już tylko cytowania zewnętrzne
				{
					cout<<(p + q)*(N[ii] - No)<<endl;
					cout<<M[ii]<<endl;
					cout<<"Przypadek 2 - wyczerpanie zbioru publikacji"<<endl;
					if((N[ii] - No) != 0) //???!!!
					{
						for (int i = 0; i < (N[ii] - No); i++)
						{
							for (int j = 0; j < (p + q); j++)
							{
								w.push_back(w[rand() % (w.size() - j)]); 
								if(j < p) //pierwsze p rozdanych cytowań traktujemy jako cytowania własne
								{
									w_autocyt.push_back(w[w.size()- 1]);
								}
								else
								{
									w_obcecyt.push_back(w[w.size()- 1]);
								}	
							}
							w.push_back(No + (i + 1)); 
						}
								
					}
					//dodawane cytowania zewnętrzne:
					for (int i = 0; i < int ((M[ii] - (N[ii] - No)*(p + q))/q); i++)
					{
						for (int j = 0; j < q; j++)
						{
							w.push_back(w[rand() % (w.size() - j)]);
							w_obcecyt.push_back(w[w.size()- 1]);
						}		
					}
					//ostatni segment: - też już bez autocytowań
					for (int j = 0; j < int ((M[ii] - (N[ii] - No)*(p + q))%q); j++)
					{
						w.push_back(w[rand() % (w.size() - j)]);
						w_obcecyt.push_back(w[w.size()- 1]);
					}
				}
						
						//wypisywanie tablic - sprawdzenie:
						/*cout<<"Tablice: "<<endl;
						for( int i = 0; i < w.size(); i++ )
						{
							cout << w[ i ]<<" ";
						}
						cout<<endl;
						for( int i = 0; i < w_autocyt.size(); i++ )
						{
							cout << w_autocyt[ i ]<<" ";
						}
						 cout<<endl;
						for( int i = 0; i < w_obcecyt.size(); i++ )
						{
							cout << w_obcecyt[ i ]<<" ";
						}
						cout<<endl;*/
						
				h_cytowania = oblicz_indeks_h(w, (-1));
				h_autocytowania = oblicz_indeks_h(w_autocyt, 0);
				h_obcecytowania = oblicz_indeks_h(w_obcecyt, 0);
				cout<<"Indeks Hirscha wynosi: "<<h_cytowania<<endl; 
				cout<<"Indeks Hirscha z autocytowan wynosi: "<<h_autocytowania<<endl; 
				cout<<"Indeks Hirscha z cytowan obcych wynosi: "<<h_obcecytowania<<endl; 
						
				indeksy[k] = h_cytowania;
				indeksy_autocyt[k] = h_autocytowania;
				indeksy_obcecyt[k] = h_obcecytowania;
						
				//czyszczenie wektorów dla tej próby:
				w.clear();
				w_autocyt.clear();
				w_obcecyt.clear();	
			}
					
			//średnie wartości indeksów h:
			double h_sr = oblicz_sredni_h(indeksy);
			double h_autocyt_sr = oblicz_sredni_h(indeksy_autocyt);
			double h_obcecyt_sr = oblicz_sredni_h(indeksy_obcecyt);
					
			//wartości niepewności:
			odchstd_h = oblicz_odchstd_h(indeksy, h_sr);
			odchstd_h_autocyt = oblicz_odchstd_h(indeksy_autocyt, h_autocyt_sr);
			odchstd_h_obcecyt = oblicz_odchstd_h(indeksy_obcecyt, h_obcecyt_sr);	
				
			//zapis otrzymanych wartości do pliku txt:
			if (myfile.is_open())
			{
				myfile << N[ii] << " " << M[ii] << " " << h_sr << " " << odchstd_h << " " << h_autocyt_sr << " " << odchstd_h_autocyt << " " << h_obcecyt_sr << " " << odchstd_h_obcecyt << "\n";
			}
			else
			{
				cout<<"Blad przy zapisie do pliku 1!"<<endl;
			}					
		}
		else
		{
			cout<<"Indeks Hirscha wynosi: "<<h_cytowania<<endl; 
			cout<<"Indeks Hirscha z autocytowan wynosi: "<<h_autocytowania<<endl; 
			cout<<"Indeks Hirscha z cytowan obcych wynosi: "<<h_obcecytowania<<endl; 
					
			//zapis otrzymanych wartości do pliku txt:
			if (myfile.is_open())
			{
				myfile << N[ii] << " " << M[ii] << " " << h_cytowania << " " << odchstd_h << " " << h_autocytowania << " " << odchstd_h_autocyt << " " << h_obcecytowania << " " << odchstd_h_obcecyt << "\n";
			}
			else
			{
				cout<<"Blad przy zapisie do pliku 2!"<<endl;
			}
		}
	}
	myfile.close();	

	
	
	return 0;
}	
