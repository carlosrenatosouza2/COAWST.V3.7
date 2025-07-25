# 📘 Git Cheat Sheet – Fluxo Básico

Guia rápido para uso do Git no dia a dia (sem Gitflow).

---

## 🚀 1️⃣ Configuração inicial (uma vez no computador)
```bash
git config --global user.name "Seu Nome"
git config --global user.email "seu@email.com"

Criar ou clonar repositório
Criar do zero:

git init
git add .
git commit -m "Primeiro commit"
git branch -M main
git remote add origin git@github.com:usuario/repositorio.git
git push -u origin main


Clonar existente:

git clone git@github.com:usuario/repositorio.git
cd repositorio

Criar branch de trabalho

git checkout -b feature/minha-tarefa

Use prefixos como feature/, bugfix/, hotfix/ para organizar

Fazer commits

git add arquivo1 arquivo2
git commit -m "Descrição clara do que foi feito"

Faça commits pequenos e objetivos.

 Atualizar branch com a main

git checkout main
git pull origin main
git checkout feature/minha-tarefa
git merge main     # ou git rebase main

Subir branch para o GitHub

git push origin feature/minha-tarefa


Merge e limpeza de branches
Depois do PR ser aprovado e mergeado:

git checkout main
git pull origin main
git branch -d feature/minha-tarefa               # remove local
git push origin --delete feature/minha-tarefa    # remove do GitHub (opcional)


Manter o repositório limpo

git branch               # lista branches locais
git branch -d nome       # apaga branch local
git fetch --prune        # limpa branches remotas apagadas



Ciclo rápido (resumão)
1️⃣ Criar branch → git checkout -b feature/minha-tarefa
2️⃣ Commitar → git add . && git commit -m "mensagem"
3️⃣ Atualizar com main → git pull origin main (merge ou rebase)
4️⃣ Subir branch → git push origin feature/minha-tarefa
5️⃣ Abrir PR → merge no GitHub
6️⃣ Apagar branch → git branch -d feature/minha-tarefa


