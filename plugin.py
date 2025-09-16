


ps = from_scala.projects()
for p in ps:
    print(p.project_name())
    print(p.course().course_name())


print(type(ps))

