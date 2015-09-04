learn-you-a-haskell
===================

Exercices and notes taken while reading the 'Learn You A Haskell' book.
Free version of the book available online [here](http://learnyouahaskell.com/introduction).

A simple way to run the exercices is to use the [official Haskell Docker container](https://hub.docker.com/_/haskell/).
```bash
docker run --rm -it --name haskell -v $(pwd):/opt/haskell-src haskell bash -c "cd /opt/haskell-src && ghci"
```