package spaken.model;

import spaken.model.Spaken;
import spaken.model.util.Unique;

public interface Element {
  public<Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp) throws Err;
  public Unique getId();
}
