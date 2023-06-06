import scala.util.Random
import scala.math.sqrt
import scala.io.Source

// ---------------------------------------------------------------------------------------------------------------------

//                     Projet Scala Tri des Kmeans          Léonard JACOB          Costa TRAN

// ---------------------------------------------------------------------------------------------------------------------
class KIris {

  // Fonction de récupération de la matrice
  def matrice() : Array[Array[Double]] =
    val source = Source.fromFile("iris.data")
    val data : Array[Array[Double]] = Array.ofDim(150,4)
    var i = 0
    for (line <- source.getLines()) do
      val det = line.split(',')
      for j <- data(0).indices do
        data(i)(j) = det(j).toDouble
      i = i + 1
    data

  // -------------------------------------------------------------------------------------------------------------------
  // Fonction de création et de repositionnement des centroids

  // Création des premiers objet dont les coordonnées sont définie de façon aléatoire
  def FirstCentroids(k : Int, mat : Array[Array[Double]]) : Array[Array[Double]] = // Nombre de Cluster et tableau de donnée (Iris) ->
    val tab: Array[Array[Double]] = Array.ofDim[Double](k, mat(0).length)
    //On stock les chiffres tiré aléatoirement pour vérifier qu'il n'y a pas deux fois les même
    val TabVerification = new Array[Int](k)
    for i<-0 until k do
      var verification = true
      var x = Random.between(0,mat.length)
      // Ca fonctionne tant que l'on ne tire pas deux chiffres égaux
      while verification do
        x = Random.between(0,mat.length)
        verification = Verification(x, TabVerification)
      tab(i)=mat(x)//on ajoute l'objet au tableau qui sera retourné
    tab

  // Met à jour les centroids selon la moyenne des valeurs attribué aux clusters
  def updateCentroids(data : Array[Array[Double]], clusters : Array[Int], nomCluster: Array[Int]): Array[Array[Double]] =
    // Tableau des nouveaux centroids
    val NewCentroids: Array[Array[Double]] = Array.ofDim(nomCluster.length, data(0).length)
    for i<-nomCluster.indices do
      var x = 0
      val ValCluster: Array[Array[Double]] = Array.ofDim[Double](clusters.length, data(0).length)
      for j<-clusters.indices do
        if clusters(j) == nomCluster(i) then
          ValCluster(x) = data(j)
          x=x+1
      // Appel de la fonction donnant la position moyenne d'un cluster
      NewCentroids(i)=moyenneCluster(ValCluster)
    NewCentroids

  // -------------------------------------------------------------------------------------------------------------------
  // Fonction de réattribution des clusters


  // Matrice, Centroids -> Tableau d'attribution des clusters
  // Définie le Cluster de chaque ligne de la matrice
  def AddToClusterAll(mat : Array[Array[Double]], Centroids : Array[Array[Double]]) : Array[Int] =
    val tabCluster = new Array[Int](mat.length)
    for i<-mat.indices do
      // attribution d'un éléments à un cluster
      tabCluster(i) = AddToClusterOne(mat(i), Centroids)
    tabCluster

  // Ligne de donnée et centroids -> Valeur du centroids le plus proche
  def AddToClusterOne(element : Array[Double], Centroids : Array[Array[Double]]) : Int = //on donne un cluster à l'élément entré en paramètre
    // Tableau des distance à chaque centroids
    val tabFinal = new Array[Double](Centroids.length)
    // Pour chaque Centroids
    for i<-Centroids.indices do
      var tmp = 0.0
      // Calcule la distance avec chaque centroids
      tmp = distance(element, Centroids(i))
      // Definition de la distance minimal
      tabFinal(i)=tmp
    Min(tabFinal) // tableau des distances aux centroids

  // -------------------------------------------------------------------------------------------------------------------
  // Fonction d'appel annexe

  // Donne la plus petite valeurs du tableau des distances aux centroids afin
  def Min(tab : Array[Double]) : Int =
    var x = tab(0)
    var j = 1
    for i<-tab.indices do
      if x>tab(i) then
        x = tab(i)
        j = i+1
    j

  // Renvoie true si un chiffre est deja dans le tableau
  def Verification(x : Int, tab : Array[Int]) : Boolean =
    for i<-tab.indices do
      if tab(i) == x then
        return true
    false

  // -------------------------------------------------------------------------------------------------------------------
  // Fonction de Calcule

  def distance(x:Array[Double], y:Array[Double]): Double = // Distance entre un point et un centroid
    var result = 0.0
    for i<-x.indices do
      var tmp = 0.0
      // Calcule de la différence
      tmp = x(i)-y(i)
      // Différence élevé au carré
      tmp = tmp*tmp
      // Ajout dans la variables somme
      result = result + tmp
    // Somme renvoyé à la racine carré
    sqrt(result)

  // faire la moyenne des éléments dans les colonnes
  def moyenneCluster(data : Array[Array[Double]]) : Array[Double] =
    val res: Array[Double] = Array.ofDim(4) // le tableau avec les moyennes
    var somme =0.0
    for j<-0 to 3 do
      somme=data(0)(j)
      var total = 0
      for i<-data.indices do
        if data(i)(j)!=0.0 then
          somme=somme+ data(i)(j)
          total = total+1
      res(j)=somme/total
    res

  def equals(tab1: Array[Int], tab2: Array[Int]):Boolean =
    for i<-tab1.indices do
      if tab1(i)!=tab2(i) then
        return false
    true

  // -------------------------------------------------------------------------------------------------------------------
  // Fonction de calcule non utilisé par le programme

  // Calcule de la Variance
  def Variance(data : Array[Array[Double]]) : Array[Double] =
    val t = data
    val m: Array[Double] = moyenneCluster(data) // récupération de la moyenne
    val res: Array[Double] = Array.ofDim(4)

    for j<-0 to 3 do
      var r=(t(0)(j)-m(j))*(t(0)(j)-m(j))
      for i<-0 to 149 do
        r=r+( (t(i)(j) -m(j))* (t(i)(j)-m(j)))
      res(j)=r/t.length
    res

  def Ecart_Type(data : Array[Array[Double]]) : Array[Double] =
    var t = data
    // Tableau contenant les variance
    val tVariance: Array[Double] = Variance(data)
    val res: Array[Double] = Array.ofDim(4)

    for j<-0 to 3 do
      // Racine carré de la Variance
      res(j)=sqrt(tVariance(j))

    res

  // -------------------------------------------------------------------------------------------------------------------
  // Différentes fonctions d'affichage des données pour des tests ou résultat

  def ClusterNumber(tab : Array[Int]): Unit =
    var x = 0
    var y = 0
    var z = 0
    for i<-tab.indices do
      if tab(i) == 1 then
        x += 1
      else if tab(i) == 2 then
        y += 1
      else
        z += 1
    println("Le cluster 1 contient: "+ x + " Valeurs")
    println("Le cluster 2 contient: "+ y + " Valeurs")
    println("Le cluster 3 contient: "+ z + " Valeurs")

  def printMat(mat : Array[Array[Double]]): Unit =
    for i<-mat.indices do
      print("\n")
      for j<-mat(i).indices do
        print(mat(i)(j).toString + "  ")

  def printTab(tab : Array[Int]): Unit =
    for i<-tab.indices do
      if i%10 == 0 then
        print("\n")
      print(tab(i).toString + "  ")

  def printTab(tab : Array[Double]): Unit =
    for i<-tab.indices do
      if i%4 == 0 then
        print("\n")
      print(tab(i).toString + "  ")

}
