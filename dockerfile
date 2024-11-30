# Utilisez une image Perl officielle comme base
FROM perl:5.34

# Installez les dépendances nécessaires (comme Tk)
RUN apt-get update && apt-get install -y \
    libx11-dev libxft-dev libjpeg-dev libpng-dev libtiff-dev libfontconfig1-dev \
    && cpanm Tk \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copiez le projet dans le conteneur
WORKDIR /usr/src/app
COPY . .

# Exposez le port si nécessaire (non requis pour Tk)
EXPOSE 8080

# Commande par défaut pour exécuter le programme
CMD ["perl", "main.pl"]
