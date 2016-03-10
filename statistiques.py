
f = open("train.csv",'r')
lignes  = f.readlines()
f.close()

base_apprentissage = []

for ligne in lignes[1:]:

	base_apprentissage.append(ligne[:-1].split(","))
	

print(base_apprentissage)