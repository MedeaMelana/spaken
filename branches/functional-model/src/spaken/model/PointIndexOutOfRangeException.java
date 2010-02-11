package spaken.model;

public class PointIndexOutOfRangeException extends RuntimeException {
  private static final String MESSAGE = "Point index out of range";
  // private final Points points;
  private final int index;
  
  public PointIndexOutOfRangeException(/*Points points, */int index) {
    super(MESSAGE);
    // this.points = points;
    this.index = index;
  }
  
}
