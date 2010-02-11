package spaken.model;

import spaken.model.util.Unique;

public interface Spaken<Elem, Err extends Throwable> {
  public Elem makePoint(Unique id, Pos pos) throws Err;
  public Elem line(Elem p1, Elem p2) throws Err;
  public Elem circle(Elem center, Elem distFrom, Elem distTo) throws Err;
  public Elem intersectCC(Elem circle1, Elem circle2) throws Err;
  public Elem intersectLL(Elem line1, Elem line2) throws Err;
  public Elem intersectLC(Elem line, Elem circle) throws Err;
  public Elem getPoint(Elem points, int i) throws Err;
}
