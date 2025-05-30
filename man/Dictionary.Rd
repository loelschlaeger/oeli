% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dictionary.R
\name{Dictionary}
\alias{Dictionary}
\title{Dictionary R6 Object}
\description{
Provides a simple key-value interface based on R6.
}
\examples{
# Managing variable metadata for a dataset

meta_dict <- Dictionary$new(
  key_name = "var_name",
  alias_name = "category",
  value_names = c("label", "type"),
  value_assert = alist(
    label = checkmate::assert_string(),
    type = checkmate::assert_choice(choices = c("numeric", "factor", "character"))
  ),
  allow_overwrite = FALSE,
  keys_reserved = c("id"),
  alias_choices = c("demographics", "outcome", "other"),
  dictionary_name = "Variable Metadata"
)

# Add entries to the dictionary
meta_dict$add(
  var_name = "age",
  label = "Age of respondent",
  type = "numeric",
  category = "demographics"
)

meta_dict$add(
  var_name = "gender",
  label = "Gender identity",
  type = "factor",
  category = "demographics"
)

meta_dict$add(
  var_name = "income",
  label = "Annual income in USD",
  type = "numeric",
  category = c("demographics", "outcome")
)

# Print dictionary
meta_dict$print()

# Retrieve full metadata for a variable
meta_dict$get("income")

# Retrieve a specific piece of metadata
meta_dict$get("income", value = "label")

# Show variables by category
meta_dict$alias
}
\seealso{
Other package helpers: 
\code{\link{Storage}},
\code{\link{check_missing}()},
\code{\link{find_namespace_calls}()},
\code{\link{identical_structure}()},
\code{\link{input_check_response}()},
\code{\link{match_arg}()},
\code{\link{package_logo}()},
\code{\link{print_data.frame}()},
\code{\link{print_matrix}()},
\code{\link{system_information}()},
\code{\link{unexpected_error}()},
\code{\link{user_confirm}()}
}
\concept{package helpers}
\keyword{indexing}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{keys}}{[\code{character()}]\cr
Available keys.}

\item{\code{alias}}{[\code{list()}]\cr
Available keys per alias value.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Dictionary-new}{\code{Dictionary$new()}}
\item \href{#method-Dictionary-add}{\code{Dictionary$add()}}
\item \href{#method-Dictionary-get}{\code{Dictionary$get()}}
\item \href{#method-Dictionary-remove}{\code{Dictionary$remove()}}
\item \href{#method-Dictionary-print}{\code{Dictionary$print()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dictionary-new"></a>}}
\if{latex}{\out{\hypertarget{method-Dictionary-new}{}}}
\subsection{Method \code{new()}}{
Initializing a new \code{Dictionary} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dictionary$new(
  key_name,
  alias_name = NULL,
  value_names = character(),
  value_assert = alist(),
  allow_overwrite = TRUE,
  keys_reserved = character(),
  alias_choices = NULL,
  dictionary_name = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key_name}}{[\code{character(1)}]\cr
The name for the key variable.}

\item{\code{alias_name}}{[\code{NULL} | \code{character(1)}]\cr
Optionally the name for the alias variable.}

\item{\code{value_names}}{[\code{character(0)}]\cr
The names of the values connected to a key.}

\item{\code{value_assert}}{[\code{alist(1)}]\cr
For each element in \code{value_names}, \code{values_assert} \emph{can} have an
identically named element of the form \code{checkmate::assert*(...)}, where
\code{...} can be any argument for the assertion function except for the
\code{x} argument.}

\item{\code{allow_overwrite}}{[\code{logical(1)}]\cr
Allow overwriting existing keys with new values?
Duplicate keys are never allowed.}

\item{\code{keys_reserved}}{[\code{character()}]\cr
Names that must not be used as keys.}

\item{\code{alias_choices}}{[\code{NULL} or \code{character()}]\cr
Optionally possible values for the alias. Can also be \code{NULL}, then all
alias values are allowed.}

\item{\code{dictionary_name}}{[\code{NULL} or \code{character()}]\cr
Optionally the name for the dictionary.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dictionary-add"></a>}}
\if{latex}{\out{\hypertarget{method-Dictionary-add}{}}}
\subsection{Method \code{add()}}{
Adding an element to the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dictionary$add(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Values for
\itemize{
\item the key variable \code{key_name} (must be a single \code{character}),
\item the alias variable \code{alias_name} (optionally, must then be a
\code{character} \code{vector}),
\item all the variables specified for \code{value_names} (if any, they must
comply to the \code{value_assert} checks).
}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dictionary-get"></a>}}
\if{latex}{\out{\hypertarget{method-Dictionary-get}{}}}
\subsection{Method \code{get()}}{
Getting elements from the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dictionary$get(key, value = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{[\code{character(1)}]\cr
A value for the key variable \code{key_name}. Use the \code{$keys} method for
available keys.}

\item{\code{value}}{[\code{NULL} | \code{character(1)}]\cr
One of the elements in \code{value_names}, selecting the required value.
Can also be \code{NULL} (default) for all values connected to the
\code{key}, returned as a \code{list}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dictionary-remove"></a>}}
\if{latex}{\out{\hypertarget{method-Dictionary-remove}{}}}
\subsection{Method \code{remove()}}{
Removing elements from the dictionary (and associated alias, if any).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dictionary$remove(key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{[\code{character(1)}]\cr
A value for the key variable \code{key_name}. Use the \code{$keys} method for
available keys.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dictionary-print"></a>}}
\if{latex}{\out{\hypertarget{method-Dictionary-print}{}}}
\subsection{Method \code{print()}}{
Printing details of the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dictionary$print()}\if{html}{\out{</div>}}
}

}
}
