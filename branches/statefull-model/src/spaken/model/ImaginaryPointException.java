package spaken.model;

import spaken.model.elements.Point;

public class ImaginaryPointException extends Exception {
  private static final String MESSAGE = "Attempt to use imaginary Point";
  private final Point point;
  
  public ImaginaryPointException(Point p) {
    super(MESSAGE);
    this.point = p;
  }
  
  public ImaginaryPointException() {
    super(MESSAGE);
    this.point = null;
  }

  public Point getPoint() {
    return point;
  }
  
}
