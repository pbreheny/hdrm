\name{spam}
\docType{data}
\alias{spam}

\title{Classification of e-mail into
spam}
\description{
The concept of unsolicited commercial e-mail, or ``spam'', is diverse
and includes such examples as advertisements for products or web sites,
get rich quick schemes, chain letters, and pornography. This is a
collection of spam and non-spam e-mails assembled by George Forman at
Hewlett-Packard in June and July of 1999. Forman, together with a team
of collaborators, also extracted 57 numeric features from the e-mails
that could potentially be used to classify the e-mails.

Note that this is a personal collection, and thus some of the features
are highly specific (e.g., the name ``George'', the phone number
650-857-7835, etc.).

}
\section{Dimensions}{
\itemize{
\item
  n = 3,000 observations
\item
  p = 57 features
}
}

\section{Outcome}{
\itemize{
\item
  \code{y} is equal to 1 if spam, 0 if not
}
}

\section{Features}{
\itemize{
\item
  48 continuous features of the form \code{word\_freq\_WORD} that
  record the percent of words in the e-mail that match WORD. For
  example, if \code{word\_freq\_you} equals 1.43, it means that 1.43\%
  of words in the e-mail are ``you''.
\item
  6 continuous features of the form \code{char\_freq\_CHAR} that
  record the percent of characters in the e-mail that match CHAR.
\item
  \code{capital\_run\_length\_average}: average length of
  uninterrupted sequences of capital letters
\item
  \code{capital\_run\_length\_longest}: length of longest
  uninterrupted sequence of capital letters
\item
  \code{capital\_run\_length\_total}: sum of length of uninterrupted
  sequences of capital letters (i.e., the total number of capital
  letters in the e-mail)
}
}

\section{Prediction set}{
\itemize{
\item
  The objects \code{Xtest} and \code{ytest} contain 1601 additional
  instances. Training and testing sets were sampled at random from the
  original data set, which contained 4601 instances.
}
}

\references{
I obtained this data set from the
\href{http://archive.ics.uci.edu/ml/datasets/Spambase/}{UCI Machine
Learning Repository}. The data set was originally created by Mark
Hopkins, Erik Reeber, George Forman, and Jaap Suermondt at
Hewlett-Packard Labs in Palo Alto, CA.
}
