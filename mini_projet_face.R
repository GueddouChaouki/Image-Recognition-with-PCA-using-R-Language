mydir = "D:/MP/PCA MINI PROJET FDD"

setwd(mydir)
##install.packages("pixmap")
library(pixmap)

######################################################################(a)
# Affiche les vues P00A + 000E + 00, P00A + 005E + 10, P00A + 005E-10 et P00A + 010E + 00 pour toutes les dossiers.
#Convertissez chaque photo sur un vecteur; Stockez la collection en tant que matrice où chaque ligne est une photo.
# La liste des images (notez l'absence de 14 signifie que 31 correspond à yaleB32)

list_img = c(1:38)  # liste de taille 38
view_list = c( 'P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00')  # les quatre vues
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)       # tous les fichie dans le dossier CroppedYale 


# pour attribuer une liste vide
data_img = vector("list",length(list_img)*length(view_list))   # data_img c'est UN VECTEUR DE taille 152= 38*4
# pour affecte une liste vide pour stocker la pgm pour le débogage
data_img_pgm = vector("list",length(list_img)*length(view_list))  # vecteur null

# Pré-allouer la matrice pour stocker les vecteurs d'image, stocker les tailles pour les calculs
Ce_visage = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm") # lire affiche une image 
plot(Ce_visage)

# La fonction getChannels génériques renvoient des matrices 
#numériques ou des tableaux contenant les canaux spécifiés.
Ce_visage_matrix = getChannels(Ce_visage) # image et de taille 192*168 

taille_originale = dim(Ce_visage_matrix)
pic_vector_length = prod(taille_originale)
pic_vector_length      # 192 * 168 = 32256
mat_img = mat.or.vec(length(list_img)*length(view_list),pic_vector_length) # 152*32256

# pour lire tous les photos et constuire la matrice de vissage original
for ( i in 1:length(list_img) ){ # 1 A 38
  for ( j in 1:length(view_list) ){  #1 A 4
    # Compile le nom de fichier correct
    Ce_filename = sprintf("CroppedYale/%s/%s_%s.pgm", dir_list_1[list_img[i]] , dir_list_1[list_img[i]] , view_list[j])
    Ce_visage = read.pnm(file = Ce_filename)
    Ce_visage_matrix = getChannels(Ce_visage)
    
    # enregetrer les pgm comme élément de la liste
    data_img_pgm[[(i-1)*length(view_list)+j]] = Ce_visage   # LE VISAGE
    # Matrice en tant qu'élément de la liste
    data_img[[(i-1)*length(view_list)+j]] = Ce_visage_matrix  # CHAQUE MATRICE DE VISAGE DANS LA LISTE
    # Faire le visage dans un vecteur et inclure dans la matrice de donnée
    # mat_img c'est une matrice de taille 152 *32256
    mat_img[(i-1)*length(view_list)+j,] =  as.vector(Ce_visage_matrix) # TRANSFORMER UN MATICE A UN VECTEUR 
  }	
}
mat_img_taille = dim(mat_img)  # La taille de matrice de tous les visage 152*32256 
print(sprintf('The matrix of all visages has taille %d par %d' , mat_img_taille[1] , mat_img_taille[2] ))

######################################################################a(b)
#calcule le "visage moyen", qui est la moyenne pour chaque pixel sur tous les visages.
# Supprimez ceci sur chacun des visages. Affichez le visage moyen comme une photo dans la taille d'origine et enregistrez une copie comme .png.

# Trouver le vecteur visage moyen
#colMeans c'est une fonction qui calcule les sommes et les moyens de la ligne et de la colonne
#pour les tableaux numériques ou les images de données

visage_moy = colMeans(mat_img) # calcule les somme myenne
# Maintenant, imprimez-le comme image
visage_moy_matrix = visage_moy
dim(visage_moy_matrix) = taille_originale
visage_moy_pix = pixmapGrey(visage_moy_matrix) # covertiren niveau de gris
plot(visage_moy_pix)

# Supprimez le visage moyen
mat_img_center = mat.or.vec(mat_img_taille[1],mat_img_taille[2])
# calcler les valeur centrer  pour tous les image
for (i in 1:mat_img_taille[1]){
  mat_img_center[i,] = mat_img[i,] - visage_moy # centre les donneé
}

####################################################################
#Utilisez prcomp () pour trouver les principaux composants de votre matrice d'image.
#Plot le nombre de composants sur l'axe des x contre la proportion de la variance expliquée sur l'axe des y.
pic_pca = prcomp(mat_img_center)
#Faire un vecteur pour stocker les variances capturées par les composants.
n_comp = length(pic_pca$x[,1])   # pour crée un vecteur de 152 case null
pca_var = mat.or.vec(n_comp,1) 
for (i in 1:n_comp){             # n_comp 1 à 152
  if (i==1){
    pca_var[i] = pic_pca$sdev[i]^2                # ecart type puisance 0= variance
  }else{
    pca_var[i] = pca_var[i-1] + pic_pca$sdev[i]^2     # variance cumuler
  }
}

pca_var = pca_var/pca_var[n_comp]*100  #pour rendre en pourcetage
# Maintenant le tracer contre le nombre de composants
plot(pca_var,ylim=c(-2,102),xlab="Nomber de Components",ylab="Percentage de Variance Captured")
# Ajoute une ligne à 100 pour afficher le niveau max.
abline(h=100,col="red")



#######################################################################(d)
# Chaque composant principal est une image, appelée "visagepropres".
# Affiche les 9 premières formes propres dans une grille 3 par 3.

visagepropre_mat = vector()
ind = 9
# Boucle les premières 9 formes propres.
Ce_visage_row = vector()  # vecteur null
for (i in 1:ind){
  # Créez le vecteur de la variable propre dans une matrice
  Ce_visagepropre = pic_pca$rotation[,i]     # les means visages
  dim(Ce_visagepropre) = taille_originale
  # pour construire la grille de visagepropre
  Ce_visage_row = cbind(Ce_visage_row,Ce_visagepropre)  # pour ajouter colone
  if ((i %% 3)==0){   # si la condition vair en cree le nv ligne sinon en ajoutant colone
    # Faire une nouvelle ligne
    visagepropre_mat = rbind(visagepropre_mat,Ce_visage_row)   # pour ajouter une ligne
    # clear row vector
    Ce_visage_row = vector()
  }
}
# Plot the visagepropres
visagepropre_pgm = pixmapGrey((visagepropre_mat-min(visagepropre_mat))/(max(visagepropre_mat)-min(visagepropre_mat))) # pour normaliser l'image
plot(visagepropre_pgm)



#######################################################################(e)
#Utilisez les visagess pour reconstruire yaleB05 P00A + 010E + 00.pgm.
# Commencez avec le visage moyen, ajoutez une seule image à la fois jusqu'à ce que vous atteigniez 24 formes propres.
# Enregistrez les résultats dans une grille de 5 par 5. Encore une fois, en commençant par le visage moyen, ajoutez cinq formes propres à la fois jusqu'à ce que vous atteigniez
# 120 visages propreistrez les résultats dans une grille 5 par 5.

# Trouvez l'index du visage 152
indice_visage = 152

# Effectue une fonction pour produire une matrice de visages 5 x 5
visagepropre_ajout_par_visage <- function(indice_visage, max_visages, par_visages, visage_moy, pic_pca, taille_originale){
  # Initialiser la matrice pour les visages ajoutés dans une seule image à la fois
  visage_par_eig_mat = vector() # vecteur nulle
  visage_par_eig_row = vector()  # vecteur nulle
  visage_par_eig_vector = visage_moy
  # Stocker le visage temporaire en tant que matrice
  visage_temp = visage_par_eig_vector    # visage_temp = mean visage
  dim(visage_temp) = taille_originale    # pour garder la mm taille de l image
  visage_par_eig_row = cbind(visage_par_eig_row,visage_temp)
  
  # Ajoutez maintenant les formes propres   par_visages=1
  for (i in 1:24){ # nb de visage
    # Trouver les indices des propres propres à inclure
    ind_include = seq((i-1)*par_visages+1,i*par_visages,1)
    # Ajoutez le vecteur qui marque [j] x autoportance [j]
    visagepropre_add = mat.or.vec(length(visage_moy),1)
    for (j in 1:length(ind_include)){
      ind_temp = ind_include[j]
      visagepropre_add = visagepropre_add + pic_pca$x[indice_visage,ind_temp]*pic_pca$rotation[,ind_temp]
    }
    visage_par_eig_vector = visage_par_eig_vector + visagepropre_add
    # Transformez-le en matrice et incluez
    visage_temp = visage_par_eig_vector
    dim(visage_temp) = taille_originale 
    ## la grille de visage_par_eig_mat  5*5
    visage_par_eig_row = cbind(visage_par_eig_row,visage_temp)
    if ((i %% 5) == 4){
      # Démarrer une nouvelle ligne
      visage_par_eig_mat = rbind(visage_par_eig_mat,visage_par_eig_row)
      visage_par_eig_row = vector()
    }
  }
  # Retourne la matrice des visages
  return(visage_par_eig_mat) 
}

# Bouclez les 24 premières formes propres
max_visages = 24
par_visages = 1
visage_par_1 = visagepropre_ajout_par_visage(indice_visage, max_visages, par_visages, visage_moy, pic_pca, taille_originale)
visage_par_1_pm = pixmapGrey(visage_par_1)

# Plot results
plot(visage_par_1_pm)

# Bouclez les 120 premières formes propres
max_visages = 120
par_visages = 5
visage_par_5 = visagepropre_ajout_par_visage(indice_visage, max_visages, par_visages, visage_moy, pic_pca, taille_originale)
visage_par_5_pm = pixmapGrey(visage_par_5)

# Plot results
plot(visage_par_5_pm)





#######################################################################(f)
#elemenner les images du sujet 01 de votre matrice d'image (il devrait y avoir quatre photos de lui) et recentrez les données.
#Rerun prcomp () pour obtenir de nouveaux composants principaux. Utilisez-les pour reconstruire yaleB01 P00A + 010E + 00.pgm.
#Do ceci en soustrayant le visage moyen et en projetant l'image restante sur les composants principaux.
#Imprimer l'image reconstruite.


# Supprime les images 1 à 4 de la matric
mat_img_mod = mat_img[setdiff(1:mat_img_taille[1],1:4),]
mat_img_mod_taille = dim(mat_img_mod)
# Recenter
visage_moy_mod = colMeans(mat_img_mod)
# Soustraire le visage moyen
mat_img_mod_centrer = mat.or.vec(mat_img_mod_taille[1],mat_img_mod_taille[2])
for (i in 1:mat_img_mod_taille[1]){
  mat_img_mod_centrer[i,] = mat_img_mod[i,] - visage_moy_mod
}
# Faites la même chose pour la photo de la requête
pic_requette = mat_img[4,]
pic_requette_centerer = pic_requette - visage_moy_mod
# Do PCA
pic_pca_mod = prcomp(mat_img_mod_centrer)
# Obtenez des scores pour la photo de requête
num_comp_mod = length(pic_pca_mod$x[,1])
scores_requette = mat.or.vec(num_comp_mod,1)
for (i in 1:num_comp_mod){
  loading_temp = pic_pca_mod$rotation[,i]
  scores_requette[i] = loading_temp %*% pic_requette_centerer
}
# Utiliser des charges pour reconstruire une image
reconst_requete = visage_moy_mod
for (i in 1:num_comp_mod){
  reconst_requete = reconst_requete + scores_requette[i]*pic_pca_mod$rotation[,i]
}
# Tracer à côté de l'original pour la comparaison
dim(reconst_requete) = taille_originale
original_requette = pic_requette
dim(original_requette) = taille_originale
cote_a_cote = cbind(original_requette,reconst_requete)
cote_a_cote_pm = pixmapGrey(cote_a_cote)
plot(cote_a_cote_pm)
filename = 'tester.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
